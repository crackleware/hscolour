-- | Formats Haskell source code using HTML with font tags.
module Language.Haskell.HsColour.HTML 
    (hscolour, 
     -- * Internals
     renderAnchors, renderComment, renderNewLinesAnchors, escape) where

import Language.Haskell.HsColour.Anchors
import Language.Haskell.HsColour.Classify as Classify
import Language.Haskell.HsColour.Colourise

import Data.Char(isAlphaNum)


-- | Formats Haskell source code using HTML with font tags.
hscolour :: ColourPrefs -- ^ Colour preferences.
         -> Bool        -- ^ Whether to include anchors.
         -> Bool        -- ^ Whether output should be partial.
         -> String      -- ^ Haskell source code.
         -> String      -- ^ Coloured Haskell source code.
hscolour pref anchor partial = 
    (if partial then id else top'n'tail)
    . pre
    . (if anchor then renderNewLinesAnchors
                      . concatMap (renderAnchors (renderToken pref))
                      . insertAnchors
                 else concatMap (renderToken pref))
    . tokenise

top'n'tail :: String -> String
top'n'tail = (htmlHeader++) . (++htmlClose)

pre :: String -> String
pre = ("<pre>"++) . (++"</pre>")

renderToken :: ColourPrefs -> (TokenType,String) -> String
renderToken pref (t,s) = fontify (colourise pref t)
                         (if t == Comment then renderComment s else escape s)

renderAnchors :: ((TokenType,String)->String)
                 -> Either String (TokenType,String) -> String
renderAnchors _      (Left v) = "<a name=\""++v++"\"></a>"
renderAnchors render (Right r) = render r

-- if there are http://links/ in a comment, turn them into
-- hyperlinks
renderComment :: String -> String
renderComment xs@('h':'t':'t':'p':':':'/':'/':_) =
        renderLink a ++ renderComment b
    where
        -- see http://www.gbiv.com/protocols/uri/rfc/rfc3986.html#characters
        isUrlChar x = isAlphaNum x || x `elem` ":/?#[]@!$&'()*+,;=-._~%"
        (a,b) = span isUrlChar xs
        renderLink link = "<a href=\"" ++ link ++ "\">" ++ escape link ++ "</a>"
        
renderComment (x:xs) = escape [x] ++ renderComment xs
renderComment [] = []

renderNewLinesAnchors :: String -> String
renderNewLinesAnchors = unlines . map render . zip [1..] . lines
    where render (line, s) = "<a name=\"line-" ++ show line ++ "\"></a>" ++ s

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

htmlHeader = "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2 Final//EN\">"
             ++"\n<html>\n"
htmlClose  = "\n</html>"
