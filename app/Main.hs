{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as TLS
import Data.ByteString.Lazy (ByteString)
import qualified Text.HTML.TagSoup.Tree as Tree
import Text.HTML.TagSoup.Tree (TagTree(..))
import qualified Text.HTML.TagSoup as Tag
import Text.HTML.TagSoup ((~==), (~/=), Tag(..))
import Text.HTML.TagSoup.Tree (TagTree)
import Control.Arrow ((&&&))
import Data.Maybe (fromJust)

parsePage :: IO [(ByteString, ByteString)]
parsePage = do
  m  <- TLS.newTlsManager
  r  <- HTTP.parseRequest "https://en.wikipedia.org/wiki/List_of_colors_(compact)"
  getColors . Tag.parseTags . HTTP.responseBody <$> HTTP.httpLbs r m
 where
  getColors = fmap ((getColor . head) &&& (getName . last))
            . fmap (\t -> [x | x@(TagBranch "p" _ _) <- Tree.universeTree t])
            . (\t -> [ cs | (TagBranch "div" _ cs) <- Tree.universeTree t ])
            . Tree.tagTree @ByteString
            . takeWhile (~/= ("<h2>" :: String))
            . tail
            . dropWhile (~/= ("<h3>" :: String))
   where
    getColor (TagBranch _ as _) = fromJust $ lookup "title" as
    getName (TagBranch _ _ t) = f . head $ concat [ cs | x@(TagBranch "a" _ cs) <- Tree.universeTree t ]
     where
      f (TagLeaf (TagText x)) = x

main :: IO ()
main = print "implement me pls"
