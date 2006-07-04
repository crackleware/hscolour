module Language.Haskell.HsColour (Output(..), ColourPrefs(..),
                                  hscolour) where

import Language.Haskell.HsColour.Colourise (ColourPrefs(..))
import qualified Language.Haskell.HsColour.TTY  as TTY
import qualified Language.Haskell.HsColour.HTML as HTML
import qualified Language.Haskell.HsColour.CSS  as CSS

data Output = TTY | HTML | CSS

hscolour :: Output      -- ^ Output format.
         -> ColourPrefs -- ^ Colour preferences for formats that support it.
         -> Bool        -- ^ Whether to include anchors.
         -> String      -- ^ Haskell source code.
         -> String      -- ^ Coloured Haskell source code.
hscolour TTY  pref _      = TTY.hscolour pref
hscolour HTML pref anchor = HTML.hscolour pref anchor
hscolour CSS  _    anchor = CSS.hscolour anchor
