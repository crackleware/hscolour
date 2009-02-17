-- | Formats Haskell source code as HTML with inline CSS.
module Language.Haskell.HsColour.InlineCSS (hscolour) where

import Language.Haskell.HsColour.Anchors
import Language.Haskell.HsColour.Classify as Classify
import Language.Haskell.HsColour.HTML (renderAnchors, renderComment,
                                       renderNewLinesAnchors, escape)

-- | Formats Haskell source code as a complete HTML document with inline styling
hscolour :: Bool   -- ^ Whether to include anchors.
         -> Bool   -- ^ Whether output should be partial
                   --   (= no document headers will be included.)
         -> String -- ^ Title for HTML page.
         -> String -- ^ Haskell source code.
         -> String -- ^ An HTML document containing the coloured 
                   --   Haskell source code.
hscolour anchor partial title =
  (if partial then id else top'n'tail title)
  . pre
  . (if anchor 
        then renderNewLinesAnchors
             . concatMap (renderAnchors renderToken)
             . insertAnchors
        else concatMap renderToken)
  . tokenise

top'n'tail :: String -> String -> String
top'n'tail title  = (cssPrefix title ++) . (++cssSuffix)

pre :: String -> String
pre =   ("<pre style=\"font-family:Consolas, Monaco, Monospace;\">"++)
      . (++"</pre>")

renderToken :: (TokenType,String) -> String
renderToken (cls,text) =
  cssStyle cls $ if cls == Comment then renderComment text else escape text

cssStyle Keyword    = stylise "color: #9a703f;"
cssStyle Keyglyph   = id
cssStyle Layout     = id
cssStyle Comment    = stylise "color: #8e9c69; font-style: italic;"
cssStyle Conid      = stylise "color: #cf694b;"
cssStyle Varid      = id
cssStyle Conop      = id
cssStyle Varop      = id
cssStyle String     = stylise "color: #5e5960;"
cssStyle Char       = stylise "color: #9a859c;"
cssStyle Number     = stylise "color: #9a859c;"
cssStyle Cpp        = id
cssStyle Error      = stylise "background-color: ##562d56;"
cssStyle Definition = stylise "color: #9a703f;"
cssStyle _          = id

stylise :: String -> String -> String
stylise s = (("<span style=\"" ++ s ++ "\">") ++) . (++ "</span>")

cssPrefix title = unlines
    ["<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"
    ,"<html>"
    ,"<head>"
    ,"<!-- Generated by HsColour, http://www.cs.york.ac.uk/fp/darcs/hscolour/ -->"
    ,"<title>"++title++"</title>"
    ,"</head>"
    ,"<body style=\"background-color: #131313; color: #ffffff;\">"
    ]
    
cssSuffix = unlines
    ["</body>"
    ,"</html>"
    ]
