import ANSI
import Classify
import Colourise
import System
import IO (hFlush,stdout)

main = do
  p <- System.getProgName
  a <- System.getArgs
  pref <- readColourPrefs
  case a of
    []          -> help p
    ["-h"]      -> help p
    ["-help"]   -> help p
    ["-tty"]    -> Prelude.interact (tty pref  . Classify.tokenise)
    ["-html"]   -> Prelude.interact (html pref . Classify.tokenise)
    ["-css"]    -> Prelude.interact (css       . Classify.tokenise)
    [a]         -> do readFile a >>= putStr . tty pref  . Classify.tokenise
    ["-tty",a]  -> do readFile a >>= putStr . tty pref  . Classify.tokenise
    ["-html",a] -> do readFile a >>= putStr . html pref . Classify.tokenise
    ["-css",a]  -> do readFile a >>= putStr . css       . Classify.tokenise
    _           -> help p
  hFlush stdout
  where
    tty pref  = concat . map renderTTY . map (\(t,s)->(s,colourise pref t))
    html pref = ("<pre>"++) . (++"</pre>")
                . concat . map renderHTML . map (\(t,s)->(s,colourise pref t))
    css = (cssPrefix++) . (++cssSuffix) . concatMap renderCSS
    help p = error ("Usage: "++p++" [-tty|-html|-css] [file.hs]")

renderTTY :: (String,[Highlight]) -> String
renderTTY (s,h) = highlight h s

renderHTML :: (String,[Highlight]) -> String
renderHTML (s,h) = fontify h (escape s)

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

renderCSS :: (TokenType,String) -> String
renderCSS (Space,text) = text
renderCSS (cls,text)   = "<span class='" ++ show cls ++ "'>"
                         ++ escape text ++ "</span>"

cssPrefix = "<html><head><link type='text/css' rel='stylesheet' href='hscolour.css'/></head><body><pre>"
cssSuffix = "</pre></body></html>"
