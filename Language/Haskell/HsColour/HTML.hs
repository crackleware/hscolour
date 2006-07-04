-- | Formats Haskell source code using HTML with font tags.
module Language.Haskell.HsColour.HTML 
    (hscolour, 
     -- * Internals
     renderAnchors, escape) where

import Language.Haskell.HsColour.Anchors
import Language.Haskell.HsColour.Classify as Classify
import Language.Haskell.HsColour.Colourise

-- | Formats Haskell source code using HTML with font tags.
hscolour :: ColourPrefs -- ^ Colour preferences.
         -> Bool        -- ^ Whether to include anchors
         -> String      -- ^ Haskell source code.
         -> String      -- ^ Coloured Haskell source code.
hscolour pref anchor = 
    top'n'tail . (if anchor then concatMap (renderAnchors (renderToken pref)) 
                                   . insertAnchors
                            else concatMap (renderToken pref)) . tokenise

top'n'tail :: String -> String
top'n'tail = ("<pre>"++) . (++"</pre>")

renderToken :: ColourPrefs -> (TokenType,String) -> String
renderToken pref (t,s) = fontify (colourise pref t) (escape s)

renderAnchors :: ((TokenType,String)->String)
                 -> Either String (TokenType,String) -> String
renderAnchors _      (Left v) = "<a name=\""++v++"\"></a>"
renderAnchors render (Right r) = render r

-- Html stuff
fontify [] s     = s
fontify (h:hs) s = font h (fontify hs s)

font Normal         s = s
font Bold           s = "<b>"++s++"</b>"
font Dim            s = "<em>"++s++"</em>"
font Underscore     s = "<u>"++s++"</u>"
font Blink          s = "<blink>"++s++"</blink>"
font ReverseVideo   s = s
font Concealed      s = s
font (Foreground c) s = "<font color="++show c++">"++s++"</font>"
font (Background c) s = "<font bgcolor="++show c++">"++s++"</font>"

escape ('<':cs) = "&lt;"++escape cs
escape ('>':cs) = "&gt;"++escape cs
escape ('&':cs) = "&amp;"++escape cs
escape (c:cs)   = c: escape cs
escape []       = []
