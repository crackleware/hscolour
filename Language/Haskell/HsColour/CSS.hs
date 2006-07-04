-- | Formats Haskell source code as HTML with CSS.
module Language.Haskell.HsColour.CSS (hscolour, hscolourFragment) where

import Language.Haskell.HsColour.Anchors
import Language.Haskell.HsColour.Classify as Classify
import Language.Haskell.HsColour.HTML (renderAnchors, escape)

-- | Formats Haskell source code as a complete HTML document with CSS.
hscolour :: Bool   -- ^ Whether to include anchors
         -> String -- ^ Haskell source code.
         -> String -- ^ An HTML document containing the coloured 
                   --   Haskell source code.
hscolour anchor = top'n'tail . hscolourFragment anchor

-- | Formats Haskell source code as an HTML fragment with CSS.
--   No stylesheet link is included in the output.
hscolourFragment :: Bool   -- ^ Whether to include anchors
                 -> String -- ^ Haskell source code.
                 -> String -- ^ An HTML fragment containing the coloured 
                           --   Haskell source code.
hscolourFragment anchor = 
    pre . (if anchor 
           then concatMap (renderAnchors renderToken) . insertAnchors
           else concatMap renderToken) . tokenise

top'n'tail :: String -> String
top'n'tail  = (cssPrefix++) . (++cssSuffix)

pre :: String -> String
pre = ("<pre>"++) . (++"</pre>")

renderToken :: (TokenType,String) -> String
renderToken (Space,text) = text
renderToken (cls,text)   = "<span class='" ++ cssClass cls ++ "'>"
                             ++ escape text ++ "</span>"

cssClass Keyword  = "keyword"
cssClass Keyglyph = "keyglyph"
cssClass Layout   = "layout"
cssClass Comment  = "comment"
cssClass Conid    = "conid"
cssClass Varid    = "varid"
cssClass Conop    = "conop"
cssClass Varop    = "varop"
cssClass String   = "str"
cssClass Char     = "chr"
cssClass Number   = "num"
cssClass Error    = "sel"

cssPrefix = "<html><head><link type='text/css' rel='stylesheet' href='hscolour.css'/></head><body>"
cssSuffix = "</body></html>"
