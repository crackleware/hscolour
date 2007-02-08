module Main where

import Language.Haskell.HsColour
import Language.Haskell.HsColour.Colourise (readColourPrefs)

import System
import IO (hPutStrLn,hFlush,stdout,stderr,hSetBuffering,BufferMode(..))
import Monad (when)
import List  (intersperse)

version = "1.7"

-- | Command-line options
data Option =
    Help		-- ^ print usage message
  | Version		-- ^ report version
  | Format Output	-- ^ what type of output to produce
  | Anchors Bool	-- ^ whether to add anchors
  | Partial Bool	-- ^ whether to produce a full document or partial
  | Input FilePath	-- ^ input source file
  deriving Eq

optionTable :: [(String,Option)]
optionTable = [ ("-help",    Help)
              , ("-version", Version)
              , ("-html",   Format HTML)
              , ("-css",    Format CSS)
              , ("-tty",    Format TTY)
              , ("-latex",  Format LaTeX)
              , ("-anchor",    Anchors True)
              , ("-noanchor",  Anchors False)
              , ("-partial",   Partial True)
              , ("-nopartial", Partial False)
              ]

parseOption :: String -> Either String Option
parseOption s@('-':_) = maybe (Left s) Right (lookup s optionTable)
parseOption s         = Right (Input s)

main :: IO ()
main = do
  prog <- System.getProgName
  args <- System.getArgs
  pref <- readColourPrefs
  let options = map parseOption args
      bad     = [ o | Left o <- options ]
      good    = [ o | Right o <- options ]
      formats = [ f | Format f <- good ]
      output    = useDefault TTY         id           formats
      ioWrapper = useDefault ttyInteract fileInteract [ f | Input f <- good ]
      anchors   = useDefault False       id           [ b | Anchors b <- good ]
      partial   = useDefault False       id           [ b | Partial b <- good ]
  when (not (null bad))
       (errorOut ("Unrecognised option(s): "++unwords bad++"\n"++usage prog))
  when (Help `elem` good)    (do putStrLn (usage prog); exitSuccess)
  when (Version `elem` good) (do putStrLn (prog++" "++version); exitSuccess)
  when (length formats > 1)
       (errorOut ("Can only choose one output format at a time: "
                  ++unwords (map show formats)))
  ioWrapper (hscolour output pref anchors partial)
  hFlush stdout

  where
    fileInteract f u = do readFile f >>= putStr . u
    ttyInteract s = do hSetBuffering stdout NoBuffering >> Prelude.interact s
    exitSuccess = exitWith ExitSuccess
    errorOut s = hPutStrLn stderr s >> hFlush stderr >> exitFailure
    usage prog = "Usage: "++prog
                 ++" options [file.hs]\n    where options = [ "
                 ++ (indent 20 . unwords . width 58 58 . intersperse "|"
                     . map fst) optionTable ++ " ]"
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
