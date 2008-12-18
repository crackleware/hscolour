-- | This is a library which colourises Haskell code.
--   It currently has five output formats:
--
-- * ANSI terminal codes
--
-- * LaTeX macros
--
-- * HTML with font tags
--
-- * HTML with CSS.
--
-- * mIRC chat client colour codes.
--
module Language.Haskell.HsColour (Output(..), ColourPrefs(..),
                                  hscolour) where

import Language.Haskell.HsColour.Colourise (ColourPrefs(..))
import qualified Language.Haskell.HsColour.TTY   as TTY
import qualified Language.Haskell.HsColour.HTML  as HTML
import qualified Language.Haskell.HsColour.CSS   as CSS
import qualified Language.Haskell.HsColour.LaTeX as LaTeX
import qualified Language.Haskell.HsColour.MIRC  as MIRC
import Data.List(mapAccumL, isPrefixOf) 
import Data.Maybe
import Options
import Language.Haskell.Output
import Debug.Trace

-- | Colourise Haskell source code with the given output format.
hscolour :: Output      -- ^ Output format.
         -> ColourPrefs -- ^ Colour preferences (for formats that support them).
         -> Bool        -- ^ Whether to include anchors.
         -> Bool        -- ^ Whether output document is partial or complete.
         -> Maybe Option -- ^ Whether input document is literate haskell or not
         -> String	-- ^ Title for output.
         -> String      -- ^ Haskell source code.
         -> String      -- ^ Coloured Haskell source code.
hscolour output pref anchor partial literate title
    | literate == Just Bird = literateHandler (map lhsClassify)
    | literate == Just TeX = 
        literateHandler (snd . mapAccumL decideTypeOfLine False)
    | isNothing literate = hscolour' output pref anchor partial title
  where
    literateHandler f = concatMap chunk . joinL . f . inlines
    chunk (Literate c) = c
    chunk (Code c)     = hscolour' output pref anchor True title c

hscolour' :: Output      -- ^ Output format.
          -> ColourPrefs -- ^ Colour preferences (for formats that support them).
          -> Bool        -- ^ Whether to include anchors.
          -> Bool        -- ^ Whether output document is partial or complete.
          -> String      -- ^ Title for output.
          -> String      -- ^ Haskell source code.
          -> String      -- ^ Coloured Haskell source code.
hscolour' TTY   pref _      _       _   = TTY.hscolour pref
hscolour' MIRC  pref _      _       _   = MIRC.hscolour pref
hscolour' LaTeX pref _      partial _   = LaTeX.hscolour pref partial
hscolour' HTML  pref anchor partial top = HTML.hscolour pref anchor partial top
hscolour' CSS   _    anchor partial top = CSS.hscolour anchor partial top

-- | Separating literate files into code\/comment chunks.
data Literate = Code {unL :: String} | Literate {unL :: String} deriving (Show)

-- Re-implementation of 'lines', for better efficiency (but decreased laziness).
-- Also, importantly, accepts non-standard DOS and Mac line ending characters.
-- And retains the trailing '\n' character in each resultant string.
inlines :: String -> [String]
inlines s = lines' s id
  where
  lines' []             acc = [acc []]
  lines' ('\^M':'\n':s) acc = acc ['\n'] : lines' s id	-- DOS
--lines' ('\^M':s)      acc = acc ['\n'] : lines' s id	-- MacOS
  lines' ('\n':s)       acc = acc ['\n'] : lines' s id	-- Unix
  lines' (c:s)          acc = lines' s (acc . (c:))

-- Note, I just pass the > symbol to the colouriser, which assumes that
-- it's token based and not parse-based!!!
lhsClassify :: String -> Literate
lhsClassify ('>':xs)  = Code ('>':xs)
lhsClassify xs        = Literate xs

-- texstyle is a bool indicating whether we are currently inside a code block
decideTypeOfLine texStyle current_line 
 | isPrefix "\\begin{code}" = codeLine
 | texStyle = if not is_end then codeLine else (False, Code (current_line ))
 | otherwise = (False, Literate current_line)
     where isPrefix = flip isPrefixOf current_line 
           codeLine = (True, Code (current_line))
           is_end = isPrefix "\\end{code}"

joinL :: [Literate] -> [Literate]
joinL []                          = []
joinL (Code c:Code c2:xs)         = joinL (Code (c++c2):xs)
joinL (Literate c:Literate c2:xs) = joinL (Literate (c++c2):xs)
joinL (any:xs)                    = any: joinL xs

