import ANSI
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
    ["-tty"]    -> Prelude.interact (tty pref)
    ["-html"]   -> Prelude.interact (html pref)
    ["-css"]    -> Prelude.interact css
    [a]         -> do readFile a >>= putStr . tty pref
    ["-tty",a]  -> do readFile a >>= putStr . tty pref
    ["-html",a] -> do readFile a >>= putStr . html pref
    ["-css",a]  -> do readFile a >>= putStr . css
    _           -> help p
  hFlush stdout
  where
    tty pref  = concat . map renderTTY . colourise pref
    html pref = ("<pre>"++) . (++"</pre>")
                . concat . map renderHTML . colourise pref
    css = (cssPrefix++) . (++cssSuffix) . concatMap renderCSS . colourise cssPref
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

cssPref = ColourPrefs
  { keyword  = [Note "keyword"]
  , keyglyph = [Note "keyglyph"]
  , layout   = [Note "layout"]
  , comment  = [Note "comment"]
  , conid    = [Note "conid"]
  , varid    = [Note "varid"]
  , conop    = [Note "conop"]
  , varop    = [Note "varop"]
  , string   = [Note "str"]
  , char     = [Note "chr"]
  , number   = [Note "num"]
  , selection = [Note "sel"]
  , variantselection = [Note "varsel"]
  }

renderCSS :: (String,[Highlight]) -> String
renderCSS (text,[Note cls]) = "<span class='" ++ cls ++ "'>" ++ escape text ++ "</span>"
renderCSS (text,[Normal]) = escape text


cssPrefix = "<html><head><link type='text/css' rel='stylesheet' href='hscolour.css'/></head><body><pre>"
cssSuffix = "</pre></body></html>"
