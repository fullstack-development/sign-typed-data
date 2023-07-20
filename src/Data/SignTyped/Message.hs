module Data.SignTyped.Message
    ( Message (..)
    , makeMessage
    , makeMessageWith
    , messageChainId
    , hashMessage
    , signMessage
    , recoverMessageSigner
    )
where

import Data.Monoid
import Data.SignTyped.Class
import Data.SignTyped.Crypto
import Data.SignTyped.EIP712Domain
import Data.SignTyped.Hashing
import Data.SignTyped.Json
import Data.SignTyped.Structure
import Data.SignTyped.Type.HList
import Data.SignTyped.Type.SName
import Data.SignTyped.Type.Some
import Data.SignTyped.Util
import Data.Type.Equality
import qualified Crypto.Secp256k1.Recovery as C
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as Aeson.Encoding
import qualified Data.Aeson.Types as Aeson.Types
import qualified Data.ByteString.Lazy as BS.Lazy
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding



{-| A data structure that combines the message to sign, its domain and their type information.

    It corresponds directly to the @TypedData@ parameter of the @eth_signTypedData@ request,
and its JSON instances map directly to that.

    As an emergent generalization, this structure allows the signable object to be any Solidity type
with a defined @encodeData@, not just structs. This includes atomic types (such as @uint256@ and @bytes@) and
arrays (e.g. @bytes[4][]@). -}
data Message = forall ctx primRep. Message
    { messageTypeContext :: TypeContext ctx
    , messageDomainValue :: EthValue ctx ('EthReprStruct EIP712DomainSymbol)
    , messagePrimaryType :: EthType primRep
    , messagePrimaryValue :: EthValue ctx primRep
    }



{-| Given a domain and a signable object, encode them into a `Message`.

    This function uses a `SignableValueDefault` instance to define the exact encoding. To choose a different
encoding, use `makeMessageWith`. -}
makeMessage ::
    forall a ethType.
    (SignableValueDefault ethType a) =>
    SomeEIP712Domain ->
    a ->
    Either String Message
makeMessage =
    makeMessageWith signableValueDefaultHandle

{-| Given a domain and a signable object, encode them into a `Message`.

    This function uses an explicitly given `SignableValueHandle` to define the exact encoding. To use the default
encoding for the given type, you can use `makeMessage` instead. -}
makeMessageWith ::
    forall a.
    SignableValueHandle a ->
    SomeEIP712Domain ->
    a ->
    Either String Message
makeMessageWith messageHandle (SomeEIP712Domain @opt domain) message =
    withSome joinedTypeContext $ \tc ->
    withSome (signableValueEthType messageHandle) $ \messageEthType ->
    runEmbed tc $ do
        domainEthValue <- signableValueEmbed domainHandle eip712DomainEthType domain
        messageEthValue <- signableValueEmbed messageHandle messageEthType message
        pure $ Message tc domainEthValue messageEthType messageEthValue
  where
    joinedTypeContext =
        signableValueTypeContext messageHandle
      <>
        signableValueTypeContext domainHandle
    domainHandle =
        signableValueHandle @EIP712DomainSymbol @(EIP712Domain opt)



instance Show Message where
    showsPrec _ message =
        showString "<Message: " .
        showString (Text.unpack $ Text.Encoding.decodeUtf8 $ BS.Lazy.toStrict $ Aeson.encode message) .
        showString ">"

instance Eq Message where
    msg1 == msg2
        | Message tc1 domainEthValue1 messageEthType1 messageEthValue1 <- msg1
        , Message tc2 domainEthValue2 messageEthType2 messageEthValue2 <- msg2
        , Just Refl <- testEquality tc1 tc2
        , eqEthValue tc1 eip712DomainEthType domainEthValue1 domainEthValue2
        , Just Refl <- testEquality messageEthType1 messageEthType2
        , eqEthValue tc1 messageEthType1 messageEthValue1 messageEthValue2 =
            True
        | otherwise =
            False

instance Aeson.ToJSON Message where
    toJSON (Message tc domainEthValue messageEthType messageEthValue) =
        Aeson.object
            [ "types" Aeson..= toUnmarkedTypeContext tc
            , "primaryType" Aeson..= toUnmarkedEthType messageEthType
            , "domain" Aeson..= ethValueToJSON tc eip712DomainEthType domainEthValue
            , "message" Aeson..= ethValueToJSON tc messageEthType messageEthValue
            ]
    toEncoding (Message tc domainEthValue messageEthType messageEthValue) =
        Aeson.Encoding.pairs $
            "types" Aeson..= toUnmarkedTypeContext tc
          <>
            "primaryType" Aeson..= toUnmarkedEthType messageEthType
          <>
            Aeson.Encoding.pair "domain" (ethValueToEncoding tc eip712DomainEthType domainEthValue)
          <>
            Aeson.Encoding.pair "message" (ethValueToEncoding tc messageEthType messageEthValue)

instance Aeson.FromJSON Message where
    parseJSON = Aeson.withObject "EIP-712 Sign Request" $ \obj -> do
        Some tc <- parseMemberWith parseUnmarkedTypeContext obj "types"
        domainEthValue <- parseMemberWith (ethValueParseJSON tc eip712DomainEthType) obj "domain"
        (_, Some messageEthType) <- parseMemberWith parseUnmarkedEthType obj "primaryType"
        messageEthValue <- parseMemberWith (ethValueParseJSON tc messageEthType) obj "message"
        pure $ Message tc domainEthValue messageEthType messageEthValue

parseMemberWith ::
    (Aeson.FromJSON t) =>
    (t -> Aeson.Types.Parser b) ->
    Aeson.Object ->
    Aeson.Key ->
    Aeson.Types.Parser b
parseMemberWith fn obj key = do
    x <- obj Aeson..: key
    fn x Aeson.<?> Aeson.Types.Key key



{-| Retrieve the chainId from the message's domain, if present. -}
messageChainId ::
    Message ->
    Maybe Int
messageChainId (Message tc (EthValueStruct domainFields) _ _) = do
    withStructDef tc (SName @EIP712DomainSymbol) $ \memDefs ->
        getAlt $
        hfoldMap
            (\case
                Pair mdef mval
                    | MemberDef (EthTypeAtomic (AtomicEthTypeInteger _ _)) "chainId" <- mdef
                    , EthValueAtomic cid <- mval
                    , toInteger (minBound @Int) <= cid && cid <= toInteger (maxBound @Int) -> do
                        Alt $ Just $ fromIntegral cid
                    | otherwise ->
                        Alt Nothing
            )
            (hzip memDefs domainFields)



{-| Calculates the final hash of the message, as given by the expression:

    @
        keccak256("\\x19\\x01" ‖ domainSeparator ‖ hashStruct(message))
    @ -}
hashMessage ::
    Message ->
    MessageHash
hashMessage (Message tc domainEthValue messageEthType messageEthValue) =
    case C.msg msgBytes of
        Nothing -> error "unexpected"
        Just msg -> msg
  where
    msgBytes =
        keccak256 $
            "\x19\x01" <>
            hashEthValue tc eip712DomainEthType domainEthValue <>
            hashEthValue tc messageEthType messageEthValue

{-| Sign a message with the given secret key. -}
signMessage ::
    C.Ctx ->
    C.SecKey ->
    Message ->
    Signature
signMessage ctx secKey message =
    messageHashSign ctx secKey (messageChainId message) (hashMessage message)

{-| Recovers the signer's address from a message and a signature. -}
recoverMessageSigner ::
    C.Ctx ->
    Message ->
    Signature ->
    Maybe Address
recoverMessageSigner ctx message sigBytes =
    messageHashRecover ctx (messageChainId message) (hashMessage message) sigBytes
