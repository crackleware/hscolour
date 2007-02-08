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
import qualified Language.Haskell.HsColour.TTY  as TTY
import qualified Language.Haskell.HsColour.HTML as HTML
import qualified Language.Haskell.HsColour.CSS  as CSS
import qualified Language.Haskell.HsColour.LaTeX  as LaTeX

-- | The supported output formats.
data Output = TTY   -- ^ ANSI terminal codes
            | LaTeX -- ^ TeX macros
            | HTML  -- ^ HTML with font tags
            | CSS   -- ^ HTML with CSS.
  deriving (Eq,Show)

-- | Colourise Haskell source code with the given output format.
hscolour :: Output      -- ^ Output format.
         -> ColourPrefs -- ^ Colour preferences for formats that support it.
         -> Bool        -- ^ Whether to include anchors.
         -> String      -- ^ Haskell source code.
         -> String      -- ^ Coloured Haskell source code.
hscolour TTY   pref _      = TTY.hscolour pref
hscolour LaTeX pref _      = LaTeX.hscolour pref
hscolour HTML  pref anchor = HTML.hscolour pref anchor
hscolour CSS   _    anchor = CSS.hscolour anchor
