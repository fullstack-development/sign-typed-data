# sign-typed-data

This library is an implementation of [EIP-712: Typed structured data hashing and signing](https://eips.ethereum.org/EIPS/eip-712) for Haskell.

It supports the signing scheme itself, for native Haskell types and arbitrary runtime values, as well as the JSON representation of the `eth_signTypedData` requests.

An introductory example:

```hs
import GHC.Generics (Generic)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

{-  Import the SignTyped facilities -}
import Data.SignTyped.Class
    ( GenericSignable (..)
    , SigningModifier (..)
    , SignableValue
    , SignableValueDefault
    )
import Data.SignTyped.Crypto
    ( pubKeyToAddress
    )
import Data.SignTyped.EIP712Domain
    ( EIP712Domain (..)
    , SomeEIP712Domain (..)
    , FMaybe (..)
    )
import Data.SignTyped.Message
    ( Message
    , makeMessage
    , recoverMessageSigner
    , signMessage
    )
import Data.SignTyped.Util
    ( encodeChecksumAddress
    , encodeHexByteString
    , readChecksumAddress
    )

{-  We use "secp256k1-haskell-recovery" for cryptography -}
import Crypto.Secp256k1 as C
import Crypto.Secp256k1.Recovery as C


{-  Make our domain types. -}

newtype Address = Address { addressBytes :: BS.ByteString }
    deriving stock (Eq)
    deriving newtype (SignableValue "address")
    deriving anyclass (SignableValueDefault "address")

instance Show Address where
    show (Address b) = show $ encodeChecksumAddress b

{-  Declare the signable types. -}

data Person = Person
    { personName :: String
    , personWallet :: Address
    }
    deriving stock (Show, Eq, Generic)

data Mail = Mail
    { mailFrom :: Person
    , mailTo :: Person
    , mailContents :: String
    }
    deriving stock (Show, Eq, Generic)

{-  Define their embeddings into Solidity.
    The list of `SigningFieldModifier`s configures the embedding. -}

deriving via (
    GenericSignable
        '[  'SigningFieldModifier
                "personName"        {- choose the Haskell field to configure -}
                ('Just "name")      {- override its name in Solidity -}
                'Nothing
        ,   'SigningFieldModifier
                "personWallet"
                ('Just "wallet")
                'Nothing
        ]
        Person
  )
  instance
    SignableValue "Person" Person

deriving via (
    GenericSignable
        '[  'SigningFieldModifier "mailFrom" ('Just "from") 'Nothing
        ,   'SigningFieldModifier "mailTo" ('Just "to") 'Nothing
        ,   'SigningFieldModifier "mailContents" ('Just "contents") 'Nothing
        ]
        Mail
  )
  instance
    SignableValue "Mail" Mail

{-  Set the defined embeddings as default. -}

instance SignableValueDefault "Person" Person

instance SignableValueDefault "Mail" Mail

{-  EIP-712 domain.
    FMaybe (FNothing, FJust) type is used to configure which fields are actually present in the Solidity structure.
    Each field in turn can be of any Haskell type, provided that there is a SignableValue instance for its
corresponding Solidity type. -}

mailDomain :: Address -> SomeEIP712Domain
mailDomain contractAddress = SomeEIP712Domain $ EIP712Domain
    { eip712DomainName = {- requires SignableValue "string" -}
        FJust @String "Ether Mail"
    , eip712DomainVersion = {- requires SignableValue "string" -}
        FJust @String "1"
    , eip712DomainChainId = {- requires SignableValue "uint256" -}
        FJust @Integer 1
    , eip712DomainVerifyingContract = {- requires SignableValue "address" -}
        FJust @Address contractAddress
    , eip712DomainSalt = {- requires SignableValue "bytes32" -}
        FNothing
    }

{-  The combination of an arbitrary domain and an object is represented by `Message`.
    This function produces such a `Message` for a given contract address and a Mail.
    `makeMessage` performs some checks at runtime, such as verifying integer ranges and bytes lengths, and returns
`Left` if this validation fails. -}

mailToMessage ::
    Address ->
    Mail ->
    Message
mailToMessage contractAddress mail =
    either error id $
    makeMessage (mailDomain contractAddress) mail

{-  A helper function to convert an address from hexadecimal with checksum casing into raw bytes. -}

unAddress ::
    Text.Text ->
    Address
unAddress t =
    maybe (error $ "Invalid address: " <> show t) Address $
    readChecksumAddress t

main :: IO ()
main = do
    C.withContext $ \secp256k1Context -> do
        let testCA = unAddress "0xCcCCccccCCCCcCCCCCCcCcCccCcCCCcCcccccccC"
        let testMail = Mail
                { mailFrom = Person
                    { personName = "Cow"
                    , personWallet = unAddress "0xCD2a3d9F938E13CD947Ec05AbC7FE734Df8DD826"
                    }
                , mailTo = Person
                    { personName = "Bob"
                    , personWallet = unAddress "0xbBbBBBBbbBBBbbbBbbBbbbbBBbBbbbbBbBbbBBbB"
                    }
                , mailContents = "Hello, Bob!"
                }
        let testMessage = mailToMessage testCA testMail

        let testSecKey = "c85ef7d79691fe79573b1a7064c19c1a9819ebdbd1faaab1a8ec92344438aaf4" :: C.SecKey
        testPubKey <- pure $! C.derivePubKey secp256k1Context testSecKey
        testAddress <- pure $! pubKeyToAddress secp256k1Context testPubKey

        {-  `Message`'s JSON instances make and parse them as eth_signTyped message objects -}
        putStrLn "\nJSON signature request:"
        BS.putStr $ BS.toStrict (Aeson.encode testMessage) <> "\n"

        testSig <- pure $! signMessage secp256k1Context testSecKey testMessage
        putStrLn "\nSignature with a given key:"
        print $ encodeHexByteString testSig

        testMbRecoveredAddress <- pure $! recoverMessageSigner secp256k1Context testMessage testSig
        putStrLn "\nRecovered signer address:"
        print $ Address <$> testMbRecoveredAddress

        putStrLn "\nDo the addresses match?"
        print $ testMbRecoveredAddress == Just testAddress
```

For more advanced usage, please refer to the Haddock documentation for the modules of this package.
