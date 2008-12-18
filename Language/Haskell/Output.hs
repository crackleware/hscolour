module Language.Haskell.Output where

-- | The supported output formats.
data Output = TTY   -- ^ ANSI terminal codes
            | LaTeX -- ^ TeX macros
            | HTML  -- ^ HTML with font tags
            | CSS   -- ^ HTML with CSS.
            | MIRC  -- ^ mIRC chat clients
  deriving (Eq,Show)
