-- Partially taken from Hugs AnsiScreen.hs library:
module ANSI
  ( highlightOn
  , highlightOff
  , highlight
  , cleareol, clearbol, clearline, clearDown, clearUp, cls
  , goto
  , cursorUp, cursorDown, cursorLeft, cursorRight
  , savePosition, restorePosition
  , Highlight(..)
  , Colour(..)
  , colourCycle
  , enableScrollRegion, scrollUp, scrollDown
  , lineWrap

  , getTerminalSize	-- :: IO (Int,Int)  (width,height)
  ) where

import List (intersperse,isPrefixOf)
import Run  (runAndReadStdout)
import Char (isDigit)
import ColourHighlight


-- Basic screen control codes:

type Pos           = (Int,Int)

at        :: Pos -> String -> String
goto      :: Int -> Int -> String
home      :: String
cls       :: String

at (x,y) s  = goto x y ++ s
goto x y    = '\ESC':'[':(show y ++(';':show x ++ "H"))
home        = goto 1 1

cursorUp    = "\ESC[A"
cursorDown  = "\ESC[B"
cursorRight = "\ESC[C"
cursorLeft  = "\ESC[D"

cleareol    = "\ESC[K"
clearbol    = "\ESC[1K"
clearline   = "\ESC[2K"
clearDown   = "\ESC[J"
clearUp     = "\ESC[1J"
-- Choose whichever of the following lines is suitable for your system:
cls         = "\ESC[2J"     -- for PC with ANSI.SYS
--cls         = "\^L"         -- for Sun window

savePosition    = "\ESC7"
restorePosition = "\ESC8"


-- data Colour    -- imported from ColourHighlight
-- data Highlight -- imported from ColourHighlight

instance Enum Highlight where
  fromEnum Normal       = 0
  fromEnum Bold         = 1
  fromEnum Dim          = 2
  fromEnum Underscore   = 4
  fromEnum Blink        = 5
  fromEnum ReverseVideo = 7
  fromEnum Concealed    = 8
  fromEnum (Foreground c) = 30 + fromEnum c
  fromEnum (Background c) = 40 + fromEnum c

highlight :: [Highlight] -> String -> String
highlight attrs s = highlightOn attrs ++ s ++ highlightOff

highlightOn []     = highlightOn [Normal]
highlightOn attrs  = "\ESC["
                     ++ concat (intersperse ";" (map (show.fromEnum) attrs))
                     ++"m"
highlightOff = "\ESC[0m"


-- An infinite supply of colours.
colourCycle :: [Colour]
colourCycle = cycle [Red,Blue,Magenta,Green,Cyan]


-- Scrolling
enableScrollRegion :: Int -> Int -> String
enableScrollRegion start end = "\ESC["++show start++';':show end++"r"

scrollDown  = "\ESCD"
scrollUp    = "\ESCM"

-- Line-wrapping mode
lineWrap True  = "\ESC[7h"
lineWrap False = "\ESC[7l"

-- Find width and height of terminal screen
getTerminalSize :: IO (Int,Int)
getTerminalSize = do
    str <- runAndReadStdout "resize -u"
    let ls = lines str
    return (find "COLUMNS" ls, find "LINES" ls)
  where
    find x  []    = 0
    find x (s:ss) | x `isPrefixOf` s = read (filter isDigit s)
                  | otherwise        = find x ss
    s `containedIn` [] = False
    s `containedIn` x@(_:xs) = s `isPrefixOf` x  || s `containedIn` xs

