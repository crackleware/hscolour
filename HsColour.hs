module Main where

import Language.Haskell.HsColour
import qualified Language.Haskell.HsColour as HSColour
import Language.Haskell.HsColour.Colourise (readColourPrefs)
import Language.Haskell.HsColour.Options
import System
import IO
import Monad (when)
import List  (intersperse, isSuffixOf)
import Debug.Trace

version = "1.15"

optionTable :: [(String,Option)]
optionTable = [ ("help",    Help)
              , ("version", Version)
              , ("print-css", Information)
              , ("html",   Format HTML)
              , ("css",    Format CSS)
              , ("icss",   Format ICSS)
              , ("tty",    Format TTY)
              , ("latex",  Format LaTeX)
              , ("mirc",   Format MIRC)
              , ("lit",    LHS True)
              , ("lit-tex",LHS True)
              , ("nolit",  LHS False)
              , ("anchor",    Anchors True)
              , ("noanchor",  Anchors False)
              , ("partial",   Partial True)
              , ("nopartial", Partial False)
              ]

parseOption :: String -> Either String Option
parseOption ('-':'o':s) = Right (Output s)
parseOption s@('-':_)   = maybe (Left s) Right
                                (lookup (dropWhile (== '-') s) optionTable)
parseOption s           = Right (Input s)

main :: IO ()
main = do
  prog <- System.getProgName
  args <- System.getArgs
  pref <- readColourPrefs
  let options = map parseOption args
      bad     = [ o | Left o <- options ]
      good    = [ o | Right o <- options ]
      formats = [ f | Format f <- good ]
      outFile = [ f | Output f <- good ]
      output    = useDefault  TTY         id           formats
      anchors   = useDefault  False       id           [ b | Anchors b <- good ]
      partial   = useDefault  False       id           [ b | Partial b <- good ]
      lhs       = useDefault  Nothing     id           [ Just b | LHS b<- good ]
      title     = useDefault  "Haskell code" id        [ f | Input f   <- good ]
      ioWrapper = useDefaults (ttyInteract  outFile (guessLiterate lhs ""))
                              (fileInteract outFile)   [ (f,guessLiterate lhs f)
                                                           | Input f   <- good ]
  when (not (null bad)) $
       errorOut ("Unrecognised option(s): "++unwords bad++"\n"++usage prog)
  when (Help `elem` good)        $ writeResult [] (usage prog)
  when (Version `elem` good)     $ writeResult [] (prog++" "++version)
  when (Information `elem` good) $ writeResult outFile cssDefaults
  when (length formats > 1) $
       errorOut ("Can only choose one output format at a time: "
                 ++unwords (map show formats))
  when (length outFile > 1) $
       errorOut ("Can only have one output file at a time.")
  ioWrapper (HSColour.hscolour output pref anchors partial title)

  where
    writeResult outF s = do if null outF then putStr s
                                         else writeFile (last outF) s
                            exitSuccess
    fileInteract out inFs u = do h <- case out of
                                          []     -> return stdout
                                          [outF] -> openFile outF WriteMode
                                 mapM_ (\ (f,lit)->
                                           readFile f >>= hPutStr h . u lit)
                                       inFs
                                 hClose h
    ttyInteract []     lit u = do hSetBuffering stdout NoBuffering
                                  Prelude.interact (u lit)
    ttyInteract [outF] lit u = do c <- hGetContents stdin
                                  writeFile outF (u lit c)
    exitSuccess = exitWith ExitSuccess
    errorOut s = hPutStrLn stderr s >> hFlush stderr >> exitFailure
    usage prog = "Usage: "++prog
                 ++" options [file.hs]\n    where\n      options = [ "
                 ++ (indent 15 . unwords . width 58 58 . intersperse "|"
                     . ("-oOUTPUT":)
                     . map (('-':) . fst)) optionTable ++ " ]"
    useDefault d f list | null list = d
                        | otherwise = f (head list)
    useDefaults d f list | null list = d
                         | otherwise = f list
    guessLiterate Nothing  f = ".lhs" `isSuffixOf` f || ".ly" `isSuffixOf` f
                               || ".lx" `isSuffixOf` f
    guessLiterate (Just b) _ = b

-- some simple text formatting for usage messages
width n left  []    = []
width n left (s:ss) = if size > left then "\n":s : width n n             ss
                                     else      s : width n (left-size-1) ss
  where size = length s

indent n [] = []
indent n ('\n':s) = '\n':replicate n ' '++indent n s
indent n (c:s)    = c: indent n s

-- Rather than have a separate .css file, define some reasonable defaults here.
cssDefaults = "\
\.hs-keyglyph, .hs-layout {color: red;}\n\
\.hs-keyword {color: blue;}\n\
\.hs-comment, .hs-comment a {color: green;}\n\
\.hs-str, .hs-chr {color: teal;}\n\
\.hs-keyword, .hs-conid, .hs-varid, .hs-conop, .hs-varop, .hs-num, \
\.hs-cpp, .hs-sel, .hs-definition {}\n\
\"
