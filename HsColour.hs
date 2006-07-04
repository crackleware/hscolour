module Main where

import Language.Haskell.HsColour
import Language.Haskell.HsColour.Colourise (readColourPrefs)

import System
import IO (hFlush,stdout)

main :: IO ()
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
  ioWrapper (hscolour output pref anchors)
  hFlush stdout
  where
    fileInteract f u = do readFile f >>= putStr . u
    help p = error ("Usage: "++p++" [-tty|-html|-css|-anchor|-anchorCSS] [file.hs]")
