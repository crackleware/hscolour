module Language.Haskell.HsColour.Colourise
  ( module Language.Haskell.HsColour.ColourHighlight
  , ColourPrefs(..)
  , readColourPrefs
  , colourise
  ) where

import Language.Haskell.HsColour.ColourHighlight
import Language.Haskell.HsColour.Classify (TokenType(..))

import System (getEnv)
import List

-- | Colour preferences.
data ColourPrefs = ColourPrefs
  { keyword, keyglyph, layout, comment
  , conid, varid, conop, varop
  , string, char, number, cpp
  , selection, variantselection, definition :: [Highlight]
  } deriving (Eq,Show,Read)

defaultColourPrefs = ColourPrefs
  { keyword  = [Foreground Green,Underscore]
  , keyglyph = [Foreground Red]
  , layout   = [Foreground Cyan]
  , comment  = [Foreground Blue]
  , conid    = [Normal]
  , varid    = [Normal]
  , conop    = [Foreground Red,Bold]
  , varop    = [Foreground Cyan]
  , string   = [Foreground Magenta]
  , char     = [Foreground Magenta]
  , number   = [Foreground Magenta]
  , cpp      = [Foreground Magenta,Dim]
  , selection = [Bold, Foreground Magenta]
  , variantselection = [Dim, Foreground Red, Underscore]
  , definition = [Foreground Blue]
  }

-- NOTE, should we give a warning message on a failed reading?
parseColourPrefs :: String -> IO ColourPrefs
parseColourPrefs x =
    case reads x of
        (res,_):_ -> return res
        _ -> return defaultColourPrefs

readColourPrefs :: IO ColourPrefs
readColourPrefs = catch
  (do val <- readFile ".hscolour"
      parseColourPrefs val)
  (\_-> catch
    (do home <- getEnv "HOME"
        val <- readFile (home++"/.hscolour")
        parseColourPrefs val)
    (\_-> return defaultColourPrefs))

-- Convert classification to colour highlights.
colourise :: ColourPrefs -> TokenType -> [Highlight]
colourise pref Space    = [Normal]
colourise pref Comment  = comment pref
colourise pref Keyword  = keyword pref
colourise pref Keyglyph = keyglyph pref
colourise pref Layout   = layout pref
colourise pref Conid    = conid pref
colourise pref Varid    = varid pref
colourise pref Conop    = conop pref
colourise pref Varop    = varop pref
colourise pref String   = string pref
colourise pref Char     = char pref
colourise pref Number   = number pref
colourise pref Cpp      = cpp pref
colourise pref Error    = selection pref
colourise pref Definition = definition pref

