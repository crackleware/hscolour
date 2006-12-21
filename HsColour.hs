module Main where

import Language.Haskell.HsColour
import Language.Haskell.HsColour.Colourise (readColourPrefs)

import System
import IO (hPutStrLn,hFlush,stdout,stderr,hSetBuffering,BufferMode(..))

version = "1.6"

main :: IO ()
main = do
  prog <- System.getProgName
  args <- System.getArgs
  pref <- readColourPrefs
  (ioWrapper,output,anchors) <- case args of
        []                -> errorOut (help prog)
        ["-h"]            -> errorOut (help prog)
        ["-help"]         -> errorOut (help prog)
        ["--help"]        -> errorOut (help prog)
        ["-v"]            -> errorOut (prog++" "++version)
        ["-version"]      -> errorOut (prog++" "++version)
        ["--version"]     -> errorOut (prog++" "++version)
        ["-tty"]          -> return (ttyInteract, TTY,  False)
        ["-html"]         -> return (ttyInteract, HTML, False)
        ["-css"]          -> return (ttyInteract, CSS,  False)
        ["-latex"]        -> return (ttyInteract, LaTeX,False)
        ["-anchorHTML"]   -> return (ttyInteract, HTML, True)
        ["-anchorCSS"]    -> return (ttyInteract, CSS,  True)
        [a]               -> return (fileInteract a, TTY,  False)
        ["-tty",a]        -> return (fileInteract a, TTY,  False)
        ["-html",a]       -> return (fileInteract a, HTML, False)
        ["-css",a]        -> return (fileInteract a, CSS,  False)
        ["-latex",a]      -> return (fileInteract a, LaTeX,False)
        ["-anchorHTML",a] -> return (fileInteract a, HTML, True)
        ["-anchorCSS",a]  -> return (fileInteract a, CSS,  True)
        _                 -> errorOut (help prog)
  ioWrapper (hscolour output pref anchors)
  hFlush stdout
  where
    fileInteract f u = do readFile f >>= putStr . u
    ttyInteract s = do hSetBuffering stdout NoBuffering >> Prelude.interact s
    errorOut s = hPutStrLn stderr s >> hFlush stderr >> exitFailure
    help p = "Usage: "++p++" [-tty|-html|-css|-latex|-anchorHTML|-anchorCSS] [file.hs]"
