{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Printer where

import qualified Dhall.Map as M
import Dhall.Core (Expr(..), Chunks(..))
import Dhall.TypeCheck (X)
import qualified Dhall.Pretty as DP
import qualified Data.Text.Prettyprint.Doc as P
import qualified Data.Text.Prettyprint.Doc.Render.Text as P
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isAlphaNum, toUpper)
import Data.Foldable (fold)
import Lens.Micro.Platform ((%~), _head, _tail, each)
import Data.Monoid (Endo(..))
import Control.Monad ((<=<))

import Types (Color(..), RGB(..), Hex(..))

-- render a list of colors to pretty-printed Dhall
render :: [Color] -> Text
render = P.renderStrict . P.layoutPretty DP.layoutOpts . DP.prettyExpr @X . colors

colors :: [Color] -> Expr s a
colors cs = RecordLit $ M.fromList z
 where
  z = zip (fmap (camelCase . colorName) cs) (fmap color cs)

color :: Color -> Expr s a
color Color{..} = RecordLit asMap
 where
  asMap = M.fromList
    [ ("rgb", rgb colorRGB)
    , ("hex", hex colorHex)
    , ("name", text colorName)
    ]

rgb :: RGB -> Expr s a
rgb (RGB r g b) = RecordLit asMap
 where
  asMap = M.fromList $ zip ["r", "g", "b"] (fmap NaturalLit [r, g, b])

hex :: Hex -> Expr s a
hex (Hex t) = text t

text :: Text -> Expr s a
text = TextLit . Chunks []

camelCase :: Text -> Text
camelCase = fold
          . (_tail . each . _head %~ toUpper)
          . fmap (T.filter isAlphaNum)
          . (T.splitOn "/" <=< T.splitOn "-" <=< T.words)
          . replaceDiacritics
          . T.toLower

-- temporary hack until I find a better solution
replaceDiacritics :: Text -> Text
replaceDiacritics = appEndo $ foldMap (Endo . uncurry T.replace) rs
 where
  rs =
    [ ("é", "e")
    , ("ú", "u")
    , ("à", "a")
    ]
