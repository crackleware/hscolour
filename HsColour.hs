import ANSI
import Classify
import Colourise
import Anchors
import System
import IO (hFlush,stdout)

main = do
  prog <- System.getProgName
  args <- System.getArgs
  pref <- readColourPrefs
  let (ioWrapper,output,anchors) = case args of
        []                -> help prog
        ["-h"]            -> help prog
        ["-help"]         -> help prog
        ["-tty"]          -> (Prelude.interact, TTY,  False)
        ["-html"]         -> (Prelude.interact, HTML, False)
        ["-css"]          -> (Prelude.interact, CSS,  False)
        ["-anchorHTML"]   -> (Prelude.interact, HTML, True)
        ["-anchorCSS"]    -> (Prelude.interact, CSS,  True)
        [a]               -> (fileInteract a, TTY,  False)
        ["-tty",a]        -> (fileInteract a, TTY,  False)
        ["-html",a]       -> (fileInteract a, HTML, False)
        ["-css",a]        -> (fileInteract a, CSS,  False)
        ["-anchorHTML",a] -> (fileInteract a, HTML, True)
        ["-anchorCSS",a]  -> (fileInteract a, CSS,  True)
        _           -> help prog
  ioWrapper (top'n'tail output
            . (if anchors then concatMap (renderAnchors (render output pref))
                               . insertAnchors
                          else concatMap (render output pref) )
            . Classify.tokenise)
  hFlush stdout
  where
    fileInteract f u = do readFile f >>= putStr . u
    help p = error ("Usage: "++p++" [-tty|-html|-css|-anchor|-anchorCSS] [file.hs]")

data Output = TTY | HTML | CSS

top'n'tail :: Output -> String -> String
top'n'tail HTML = ("<pre>"++) . (++"</pre>")
top'n'tail CSS  = (cssPrefix++) . (++cssSuffix)
top'n'tail TTY  = id

render :: Output -> ColourPrefs -> (TokenType,String) -> String
render TTY  pref (t,s)     = highlight (colourise pref t) s
render HTML pref (t,s)     = fontify (colourise pref t) (escape s)
render CSS  _ (Space,text) = text
render CSS  _ (cls,text)   = "<span class='" ++ show cls ++ "'>"
                             ++ escape text ++ "</span>"

renderAnchors :: ((TokenType,String)->String)
                 -> Either String (TokenType,String) -> String
renderAnchors render (Left v) = "<a name=\""++v++"\"></a>"
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

-- CSS stuff
instance Show TokenType where
  show Keyword  = "keyword"
  show Keyglyph = "keyglyph"
  show Layout   = "layout"
  show Comment  = "comment"
  show Conid    = "conid"
  show Varid    = "varid"
  show Conop    = "conop"
  show Varop    = "varop"
  show String   = "str"
  show Char     = "chr"
  show Number   = "num"
  show Error    = "sel"

cssPrefix = "<html><head><link type='text/css' rel='stylesheet' href='hscolour.css'/></head><body><pre>"
cssSuffix = "</pre></body></html>"
