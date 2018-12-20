module Types where

import Data.Text (Text)
import Numeric.Natural (Natural)

newtype Hex = Hex Text deriving Show

data RGB = RGB Natural Natural Natural deriving Show

data Color = Color
  { colorName :: Text
  , colorRGB  :: RGB
  , colorHex  :: Hex
  } deriving Show
