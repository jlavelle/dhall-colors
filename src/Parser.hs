{-# LANGUAGE OverloadedStrings #-}

module Parser where

import qualified Data.Text as T
import qualified Data.Text.Read as T
import Data.Text (Text)
import qualified Text.HTML.TagSoup.Tree as Tree
import Text.HTML.TagSoup.Tree (TagTree(..))
import Text.HTML.TagSoup (Tag(..))
import Control.Arrow ((&&&), (<<<))
import Control.Monad ((<=<))
import Data.Functor ((<&>))
import Data.Bitraversable (bisequenceA)
import Data.Maybe (mapMaybe)

import Types (Color(..), RGB(..), Hex(..))

-- Parse the colors on https://en.wikipedia.org/wiki/List_of_colors_(compact)
parse :: Text -> [Color]
parse = mapMaybe go . divChildren . Tree.parseTree
 where
  go = parseColor <=< nameAndColor . pElems

divChildren :: [TagTree Text] -> [[TagTree Text]]
divChildren t = [ x | (TagBranch "div" _ x) <- Tree.universeTree t ]

pElems :: [TagTree Text] -> [TagTree Text]
pElems t = [ x | x@(TagBranch "p" _ _) <- Tree.universeTree t ]

nameAndColor :: [TagTree Text] -> Maybe (Text, Text)
nameAndColor = bisequenceA <<< (getColor <=< headMay) &&& (getName <=< lastMay)

getColor :: TagTree Text -> Maybe Text
getColor (TagBranch _ as _) = lookup "title" as
getColor _ = Nothing

getName :: TagTree Text -> Maybe Text
getName (TagBranch _ _ t) = tagText <=< headMay $ hasUrl <> noUrl
 where
  hasUrl = concat [ cs | (TagBranch "a" _ cs) <- Tree.universeTree t ]
  noUrl  = [ x | x@(TagLeaf _) <- Tree.universeTree t]
getName _ = Nothing

tagText :: TagTree Text -> Maybe Text
tagText (TagLeaf (TagText x)) = Just x
tagText _ = Nothing

parseColor :: (Text, Text) -> Maybe Color
parseColor (v, n) = parseColorVals v <&> uncurry (Color n)

parseColorVals :: Text -> Maybe (RGB, Hex)
parseColorVals = go . T.words . stripParens
 where
  go [_,_,_,_,_,r,g,b,_,h] = do
    r' <- parseDecimal r
    g' <- parseDecimal g
    b' <- parseDecimal b
    pure (RGB r' g' b', Hex h)
  go _ = Nothing

parseDecimal :: Integral a => Text -> Maybe a
parseDecimal = either (const Nothing) (Just . fst) . T.decimal

stripParens :: Text -> Text
stripParens = T.filter (\x -> x /= '(' && x /= ')')

headMay :: [a] -> Maybe a
headMay []    = Nothing
headMay (a:_) = Just a

lastMay :: [a] -> Maybe a
lastMay = headMay . reverse
