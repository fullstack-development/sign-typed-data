module Data.SignTyped.Test.NormalizeJson where

import Data.Char
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BS.Lazy
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Language.Haskell.TH.Syntax as TH
import qualified Language.Haskell.TH.Quote as TH

{-| Remove all spaces from the string, except those inside quoted literals. -}
normalizeJson :: String -> BS.Lazy.ByteString
normalizeJson = BS.Lazy.pack . goJson
  where
    goJson = \case
        [] -> []
        '"' : rest -> 34 : goString rest
        c : rest
            | isSpace c -> goJson rest
            | otherwise -> prependCharUtf8 c $ goJson rest
    goString = \case
        [] -> []
        '"' : rest -> 34 : goJson rest
        '\\' : '"' : rest -> 92 : 34 : goString rest
        '\\' : '\\' : rest -> 92 : 92 : goString rest
        c : rest -> prependCharUtf8 c $ goString rest
    prependCharUtf8 c rest =
        BS.foldr (:) rest (Text.Encoding.encodeUtf8 $ Text.singleton c)

njson :: TH.QuasiQuoter
njson = TH.QuasiQuoter
    { TH.quoteExp = TH.lift . normalizeJson
    , TH.quotePat = \_ -> fail "not applicable in this context"
    , TH.quoteType = \_ -> fail "not applicable in this context"
    , TH.quoteDec = \_ -> fail "not applicable in this context"
    }
