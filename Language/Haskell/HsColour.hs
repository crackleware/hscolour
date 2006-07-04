-- | This is a library with colourises Haskell code. 
--   It currently has three output formats: 
--
-- * ANSI terminal codes
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

-- | The supported output formats.
data Output = TTY  -- ^ ANSI terminal codes
            | HTML -- ^ HTML with font tags
            | CSS  -- ^ HTML with CSS.

-- | Colourise Haskell source code with the give output format.
hscolour :: Output      -- ^ Output format.
         -> ColourPrefs -- ^ Colour preferences for formats that support it.
         -> Bool        -- ^ Whether to include anchors.
         -> String      -- ^ Haskell source code.
         -> String      -- ^ Coloured Haskell source code.
hscolour TTY  pref _      = TTY.hscolour pref
hscolour HTML pref anchor = HTML.hscolour pref anchor
hscolour CSS  _    anchor = CSS.hscolour anchor
