module Main where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as TLS
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Text.Lazy as LT
import Data.Text (Text)
import qualified Data.Text.IO as T

import qualified Printer
import qualified Parser

readWiki :: IO Text
readWiki = do
  m <- TLS.newTlsManager
  r <- HTTP.parseRequest "https://en.wikipedia.org/wiki/List_of_colors_(compact)"
  LT.toStrict . LT.decodeUtf8 . HTTP.responseBody <$> HTTP.httpLbs r m

main :: IO ()
main = do
  setLocaleEncoding utf8
  (r, l) <- Printer.run . Parser.run <$> readWiki
  T.writeFile "./output/colorRecord.dhall" r
  T.writeFile "./output/colorList.dhall" l
