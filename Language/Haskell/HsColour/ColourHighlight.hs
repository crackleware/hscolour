module Language.Haskell.HsColour.ColourHighlight
  ( Colour(..)
  , Highlight(..)
  ) where

data Colour = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White
  deriving (Eq,Show,Read,Enum)

data Highlight =
    Normal
  | Bold
  | Dim
  | Underscore
  | Blink
  | ReverseVideo
  | Concealed
  | Foreground Colour
  | Background Colour
  deriving (Eq,Show,Read)

