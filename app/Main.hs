{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as TLS
import Data.ByteString.Lazy (ByteString)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.Read as T
import Data.Text.Lazy (Text)
import qualified Text.HTML.TagSoup.Tree as Tree
import Text.HTML.TagSoup.Tree (TagTree(..))
import Text.HTML.TagSoup (Tag(..))
import Text.HTML.TagSoup.Tree (TagTree)
import Control.Arrow ((&&&))
import Control.Monad ((<=<))
import Data.Bitraversable (bisequenceA)
import Data.Maybe (fromJust, catMaybes)
import Data.Functor ((<&>))
import Lens.Micro.Platform ((%~), _head, _tail, each, filtered)
import Data.Char (toUpper, isAlphaNum)
import Data.Foldable (fold)

parsePage :: IO [Color]
parsePage = do
  m <- TLS.newTlsManager
  r <- HTTP.parseRequest "https://en.wikipedia.org/wiki/List_of_colors_(compact)"
  getColors . Tree.parseTree . T.decodeUtf8 . HTTP.responseBody <$> HTTP.httpLbs r m
 where
  getColors = catMaybes
            . fmap (parseColor <=< bisequenceA . nameAndColor . pElems)
            . divChildren
   where
    divChildren t = [ x | (TagBranch "div" _ x) <- Tree.universeTree t ]
    pElems t      = [ x | x@(TagBranch "p" _ _) <- Tree.universeTree t ]
    nameAndColor  = (getColor <=< headM) &&& (getName <=< lastM)
     where
      getColor (TagBranch _ as _) = lookup "title" as
      getName (TagBranch _ _ t)   = fmap tagText . headM $ hasUrl <> noUrl
       where
        hasUrl = concat [ cs | (TagBranch "a" _ cs) <- Tree.universeTree t ]
        noUrl  = [ x | x@(TagLeaf _) <- Tree.universeTree t]
        tagText (TagLeaf (TagText x)) = x

stripParens :: Text -> Text
stripParens = T.filter (\x -> x /= '(' && x /= ')')

newtype Hex = Hex Text deriving Show

data RGB = RGB Int Int Int deriving Show

data Color = Color
  { name :: Text
  , rgb  :: RGB
  , hex  :: Hex
  } deriving Show

parseColor :: (Text, Text) -> Maybe Color
parseColor (v, n) = parseColorVals v <&> \(r, h) -> Color n r h

parseColorVals :: Text -> Maybe (RGB, Hex)
parseColorVals = go . T.words . stripParens
 where
  go [_,_,_,_,_,r,g,b,_,h] = do
    r' <- parseDecimal r
    g' <- parseDecimal g
    b' <- parseDecimal b
    pure (RGB r' g' b', Hex h)
  go _ = Nothing

parseDecimal :: Text -> Maybe Int
parseDecimal = either (const Nothing) (Just . fst) . T.decimal

camelCase :: Text -> Text
camelCase = fold . (_tail . each . _head %~ toUpper) . fmap (T.filter isAlphaNum) . T.words . T.toLower

headM :: [a] -> Maybe a
headM []    = Nothing
headM (a:_) = Just a

lastM :: [a] -> Maybe a
lastM = headM . reverse

main :: IO ()
main = print "implement me pls"
