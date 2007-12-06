module Main where

import Language.Haskell.HsColour
import Language.Haskell.HsColour.Colourise (readColourPrefs)

import System
import IO (hPutStrLn,hFlush,stdout,stderr,hSetBuffering,BufferMode(..))
import Monad (when)
import List  (intersperse)

version = "1.9"

-- | Command-line options
data Option =
    Help		-- ^ print usage message
  | Version		-- ^ report version
  | Information		-- ^ report auxiliary information, e.g. CSS defaults
  | Format Output	-- ^ what type of output to produce
  | LHS Bool		-- ^ literate input (i.e. multiple embedded fragments)
  | Anchors Bool	-- ^ whether to add anchors
  | Partial Bool	-- ^ whether to produce a full document or partial
  | Input FilePath	-- ^ input source file
  | Output FilePath	-- ^ output source file
  deriving Eq

optionTable :: [(String,Option)]
optionTable = [ ("help",    Help)
              , ("version", Version)
              , ("print-css", Information)
              , ("html",   Format HTML)
              , ("css",    Format CSS)
              , ("tty",    Format TTY)
              , ("latex",  Format LaTeX)
              , ("mirc",   Format MIRC)
              , ("lit",       LHS True)
              , ("nolit",     LHS False)
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
      fileInteract = fileInteractOut outFile
      output    = useDefault TTY         id           formats
      ioWrapper = useDefault ttyInteract fileInteract [ f | Input f <- good ]
      anchors   = useDefault False       id           [ b | Anchors b <- good ]
      partial   = useDefault False       id           [ b | Partial b <- good ]
      lhs       = useDefault False       id           [ b | LHS b <- good ] 
  when (not (null bad))
       (errorOut ("Unrecognised option(s): "++unwords bad++"\n"++usage prog))
  when (Help `elem` good)    (do putStrLn (usage prog); exitSuccess)
  when (Version `elem` good) (do putStrLn (prog++" "++version); exitSuccess)
  when (Information `elem` good) (do putStrLn cssDefaults; exitSuccess)
  when (length formats > 1)
       (errorOut ("Can only choose one output format at a time: "
                  ++unwords (map show formats)))
  when (length outFile > 1)
       (errorOut ("Can only have one output file at a time."))
  ioWrapper (hscolour output pref anchors partial lhs)
  hFlush stdout

  where
    writeResult outF = if null outF then putStr else writeFile (last outF)
    fileInteractOut outF inF u = do readFile inF >>= writeResult outF . u
    ttyInteract s = do hSetBuffering stdout NoBuffering >> Prelude.interact s
    exitSuccess = exitWith ExitSuccess
    errorOut s = hPutStrLn stderr s >> hFlush stderr >> exitFailure
    usage prog = "Usage: "++prog
                 ++" options [file.hs]\n    where\n      options = [ "
                 ++ (indent 15 . unwords . width 58 58 . intersperse "|"
                     . ("-oOUTPUT":)
                     . map (('-':) . fst)) optionTable ++ " ]"
    useDefault d f list | null list = d
                        | otherwise = f (head list)

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
\.keyglyph, .layout {color: red;}\n\
\.keyword {color: blue;}\n\
\.comment, .comment a {color: green;}\n\
\.str, .chr {color: teal;}\n\
\.keyword,.conid, .varid, .conop, .varop, .num, .cpp, .sel, .definition {}\n\
\"
