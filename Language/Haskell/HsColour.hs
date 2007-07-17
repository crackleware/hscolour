-- | This is a library with colourises Haskell code. 
--   It currently has four output formats: 
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

-- | The supported output formats.
data Output = TTY   -- ^ ANSI terminal codes
            | LaTeX -- ^ TeX macros
            | HTML  -- ^ HTML with font tags
            | CSS   -- ^ HTML with CSS.
            | MIRC  -- ^ mIRC chat clients
  deriving (Eq,Show)

-- | Colourise Haskell source code with the given output format.
hscolour :: Output      -- ^ Output format.
         -> ColourPrefs -- ^ Colour preferences for formats that support it.
         -> Bool        -- ^ Whether to include anchors.
         -> Bool        -- ^ Whether output document is partial or complete.
         -> Bool        -- ^ Whether input document is literate haskell or not
         -> String      -- ^ Haskell source code.
         -> String      -- ^ Coloured Haskell source code.
hscolour output pref anchor partial False = 
    hscolour' output pref anchor partial
hscolour output pref anchor partial True = 
    concatMap chunk . joinL . map lhsClassify . inlines
  where
    chunk (Literate c) = c
    chunk (Code c)     = hscolour' output pref anchor True c

hscolour' :: Output      -- ^ Output format.
          -> ColourPrefs -- ^ Colour preferences for formats that support it.
          -> Bool        -- ^ Whether to include anchors.
          -> Bool        -- ^ Whether output document is partial or complete.
          -> String      -- ^ Haskell source code.
          -> String      -- ^ Coloured Haskell source code.
hscolour' TTY   pref _      _       = TTY.hscolour pref
hscolour' MIRC  pref _      _       = MIRC.hscolour pref
hscolour' LaTeX pref _      partial = LaTeX.hscolour pref partial
hscolour' HTML  pref anchor partial = HTML.hscolour pref anchor partial
hscolour' CSS   _    anchor partial = CSS.hscolour anchor partial

-- | Separating literate files into code\/comment chunks.
data Literate = Code {unL :: String} | Literate {unL :: String}

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

joinL :: [Literate] -> [Literate]
joinL []                          = []
joinL (Code c:Code c2:xs)         = joinL (Code (c++c2):xs)
joinL (Literate c:Literate c2:xs) = joinL (Literate (c++c2):xs)
joinL (any:xs)                    = any: joinL xs

