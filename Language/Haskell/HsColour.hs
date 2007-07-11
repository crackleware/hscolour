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
         -> String      -- ^ Haskell source code.
         -> String      -- ^ Coloured Haskell source code.
hscolour TTY   pref _      _       = TTY.hscolour pref
hscolour MIRC  pref _      _       = MIRC.hscolour pref
hscolour LaTeX pref _      partial = LaTeX.hscolour pref partial
hscolour HTML  pref anchor partial = HTML.hscolour pref anchor partial
hscolour CSS   _    anchor partial = CSS.hscolour anchor partial
