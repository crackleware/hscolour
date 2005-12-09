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
    [a]         -> do readFile a >>= putStr . tty pref
    ["-tty",a]  -> do readFile a >>= putStr . tty pref
    ["-html",a] -> do readFile a >>= putStr . html pref
    _           -> help p
  hFlush stdout
  where
    tty pref  = concat . map renderTTY . colourise pref
    html pref = ("<pre>"++) . (++"</pre>")
                . concat . map renderHTML . colourise pref
    help p = error ("Usage: "++p++" [-tty|-html] [file.hs]")

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
