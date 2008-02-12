module Language.Haskell.HsColour.ColourHighlight
  ( Colour(..)
  , Highlight(..)
  ) where

-- | Colours supported by ANSI codes.
data Colour = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White
  deriving (Eq,Show,Read,Enum)

-- | Types of highlighting supported by ANSI codes.
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

