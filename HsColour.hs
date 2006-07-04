module Main where

import Language.Haskell.HsColour
import Language.Haskell.HsColour.Colourise (readColourPrefs)

import System
import IO (hPutStrLn,hFlush,stdout,stderr)

version = "1.4"

main :: IO ()
main = do
  prog <- System.getProgName
  args <- System.getArgs
  pref <- readColourPrefs
  (ioWrapper,output,anchors) <- case args of
        []                -> errorOut (help prog)
        ["-h"]            -> errorOut (help prog)
        ["-help"]         -> errorOut (help prog)
        ["-v"]            -> errorOut (prog++" "++version)
        ["-version"]      -> errorOut (prog++" "++version)
        ["--version"]     -> errorOut (prog++" "++version)
        ["-tty"]          -> return (Prelude.interact, TTY,  False)
        ["-html"]         -> return (Prelude.interact, HTML, False)
        ["-css"]          -> return (Prelude.interact, CSS,  False)
        ["-anchorHTML"]   -> return (Prelude.interact, HTML, True)
        ["-anchorCSS"]    -> return (Prelude.interact, CSS,  True)
        [a]               -> return (fileInteract a, TTY,  False)
        ["-tty",a]        -> return (fileInteract a, TTY,  False)
        ["-html",a]       -> return (fileInteract a, HTML, False)
        ["-css",a]        -> return (fileInteract a, CSS,  False)
        ["-anchorHTML",a] -> return (fileInteract a, HTML, True)
        ["-anchorCSS",a]  -> return (fileInteract a, CSS,  True)
        _                 -> errorOut (help prog)
  ioWrapper (hscolour output pref anchors)
  hFlush stdout
  where
    fileInteract f u = do readFile f >>= putStr . u
    errorOut s = hPutStrLn stderr s >> hFlush stderr >> exitFailure
    help p = "Usage: "++p++" [-tty|-html|-css|-anchor|-anchorCSS] [file.hs]"
