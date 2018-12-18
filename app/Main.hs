{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as TLS
import Data.ByteString.Lazy (ByteString)
import qualified Text.HTML.TagSoup.Tree as Tree
import Text.HTML.TagSoup.Tree (TagTree(..))
import qualified Text.HTML.TagSoup as Tag
import Text.HTML.TagSoup ((~==), (~/=), Tag(..))
import Text.HTML.TagSoup.Tree (TagTree)
import Control.Arrow ((&&&), (<<<))
import Control.Monad ((<=<))
import Data.Bitraversable (bisequenceA)
import Data.Maybe (fromJust, catMaybes)

parsePage :: IO [(ByteString, ByteString)]
parsePage = do
  m  <- TLS.newTlsManager
  r  <- HTTP.parseRequest "https://en.wikipedia.org/wiki/List_of_colors_(compact)"
  getColors . Tree.parseTree . HTTP.responseBody <$> HTTP.httpLbs r m
 where
  getColors = catMaybes
            . fmap (bisequenceA . getData . pElems)
            . divChildren
   where
    divChildren t = [ x | (TagBranch "div" _ x) <- Tree.universeTree t ]
    pElems t      = [ x | x@(TagBranch "p" _ _) <- Tree.universeTree t ]
    getData       = (getColor <=< headM) &&& (getName <=< lastM)
     where
      getColor (TagBranch _ as _) = lookup "title" as
      getName (TagBranch _ _ t)   = fmap tagText . headM $ hasUrl <> noUrl
       where
        hasUrl = concat [ cs | (TagBranch "a" _ cs) <- Tree.universeTree t ]
        noUrl  = [ x | x@(TagLeaf _) <- Tree.universeTree t]
        tagText (TagLeaf (TagText x)) = x

headM :: [a] -> Maybe a
headM []    = Nothing
headM (a:_) = Just a

lastM :: [a] -> Maybe a
lastM = headM . reverse

main :: IO ()
main = print "implement me pls"
