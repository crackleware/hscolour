module Language.Haskell.HsColour.TTY (hscolour) where

import Language.Haskell.HsColour.ANSI
import Language.Haskell.HsColour.Classify
import Language.Haskell.HsColour.Colourise


hscolour :: ColourPrefs -> String -> String
hscolour pref = concatMap (renderToken pref) . tokenise

renderToken :: ColourPrefs -> (TokenType,String) -> String
renderToken pref (t,s) = highlight (colourise pref t) s
