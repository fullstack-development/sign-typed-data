{-| We use "Crypto.Secp256k1.Recovery" as a back-end for the cryptographic operations. -}
module Data.SignTyped.Crypto
    ( MessageHash
    , Signature
    , Address
    , pubKeyToAddress
    , messageHashSign
    , messageHashRecover
    , messageHashVerify
    )
where

import Control.Applicative
import Data.SignTyped.Util
import Data.Word
import qualified Data.ByteString as BS
import qualified Crypto.Secp256k1.Recovery as C

type MessageHash = C.Msg

type Signature = BS.ByteString

type Address = BS.ByteString

{-| Derive an Ethereum-compatible address from a public key. -}
pubKeyToAddress ::
    C.Ctx ->
    C.PubKey ->
    Address
pubKeyToAddress ctx =
    BS.drop 12 . keccak256 . BS.drop 1 . C.exportPubKey ctx False

{- https://eips.ethereum.org/EIPS/eip-155 -}
chainIdOffset :: Int -> Word8
chainIdOffset chainId = fromIntegral (chainId * 2 + 36)

{-| Sign a given message with a given secret key. The signature produced is the 65-byte (v,r,s) recoverable signature.

    If a chainId is provided, it will be encoded to the v byte of the signature according to EIP-155, as an offset of
(chainId * 2 + 36). Otherwise, the default offset of 27 is used. -}
messageHashSign ::
    C.Ctx ->
    C.SecKey ->
    Maybe Int -> {-^ chainId -}
    MessageHash ->
    Signature
messageHashSign ctx secKey mbChainId msg = do
    let C.CompactRecSig rs vb = C.exportCompactRecSig ctx $ C.signRecMsg ctx secKey msg
    let vs = vb + maybe 27 chainIdOffset mbChainId
    BS.snoc rs vs

{-| Recover an address from a given message and a given (v,r,s) signature.

    For the v component, the default offset of 27 is always allowed. If a chainId is provided, its offset of
(chainId * 2 + 36) will also be tried.

    In the special case of @chainId = 124 + 128 k@, the value of @v = 28@ becomes ambiguous — it could mean both
the odd parity with the default offset 27, and the even parity with the chainId-specific offset of 28. In this case,
this function interprets it as the latter (the chainId-specific offset takes priority). -}
messageHashRecover ::
    C.Ctx ->
    Maybe Int -> {-^ chainId -}
    MessageHash ->
    Signature ->
    Maybe Address
messageHashRecover ctx mbChainId msg sigBytes = do
    (rs, vs) <- BS.unsnoc sigBytes
    vb <-
        (mbChainId >>= \chainId -> tryOffset (chainIdOffset chainId) vs)
      <|>
        tryOffset 27 vs
    sig <- C.importCompactRecSig ctx $ C.CompactRecSig rs vb
    pkey <- C.recover ctx sig msg
    Just $ pubKeyToAddress ctx pkey
  where
    tryOffset s vs = do
        let vb = vs - s
        if vb <= 1
            then Just vb
            else Nothing

{-| Verify that a given (v,r,s) signature on a given message corresponds to a given address.

    This function simply recovers the address with `messageHashRecover` and compares it to the one given. -}
messageHashVerify ::
    C.Ctx ->
    Maybe Int ->
    MessageHash ->
    Signature ->
    Address ->
    Bool
messageHashVerify ctx mbChainId msg sigBytes address =
    messageHashRecover ctx mbChainId msg sigBytes == Just address
