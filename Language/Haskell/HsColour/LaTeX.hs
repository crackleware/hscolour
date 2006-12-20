-- | Formats Haskell source code using LaTeX macros.
module Language.Haskell.HsColour.LaTeX (hscolour, hscolourFragment) where

import Language.Haskell.HsColour.Classify as Classify
import Language.Haskell.HsColour.Colourise

-- | Formats Haskell source code as a complete LaTeX document.
hscolour :: ColourPrefs -- ^ Colour preferences.
         -> String      -- ^ Haskell source code.
         -> String      -- ^ An LaTeX document containing the coloured 
                        --   Haskell source code.
hscolour pref = top'n'tail . hscolourFragment pref

-- | Formats Haskell source code as an LaTeX fragment.
hscolourFragment :: ColourPrefs -- ^ Colour preferences.
                 -> String -- ^ Haskell source code.
                 -> String -- ^ An LaTeX fragment containing the coloured 
                           --   Haskell source code.
hscolourFragment pref = concatMap (renderToken pref) . tokenise

top'n'tail :: String -> String
top'n'tail  = (latexPrefix++) . (++latexSuffix)

-- | Wrap each lexeme in the appropriate LaTeX macro.
--   TODO: filter dangerous characters like "{}_$"
renderToken :: ColourPrefs -> (TokenType,String) -> String
renderToken pref (Space,text) = filterSpace text
renderToken pref (cls,text)   = let 
	symb = case cls of
		String -> "``" ++ (reverse . tail . reverse . tail $ text) ++ "''"
		_      -> text
	style = colourise pref cls
	(pre, post) = unzip $ map latexHighlight style in
	(concat pre) ++ filterSpecial symb ++ (concat post) 

-- | Filter white space characters.
filterSpace :: String
            -> String
filterSpace ('\n':ss) = '\\':'\\':(filterSpace ss)
filterSpace (' ':ss)  = "\\hsspace "++(filterSpace ss)
filterSpace ('\t':ss) = "\\hstab "++(filterSpace ss)
filterSpace (c:ss)    = c:(filterSpace ss)
filterSpace []        = []

-- | Filters the characters "#$%&~_^\{}" which are special
--   in LaTeX.
filterSpecial :: String  -- ^ The string to filter. 
              -> String  -- ^ The LaTeX-safe string.
filterSpecial ('#':cs)  = '\\':'#':(filterSpecial cs)
filterSpecial ('$':cs)  = '\\':'$':(filterSpecial cs)
filterSpecial ('%':cs)  = '\\':'%':(filterSpecial cs)
filterSpecial ('&':cs)  = '\\':'&':(filterSpecial cs)
filterSpecial ('~':cs)  = "\\tilde{ }"++(filterSpecial cs)
filterSpecial ('_':cs)  = '\\':'_':(filterSpecial cs)
filterSpecial ('^':cs)  = "\\hat{ }"++(filterSpecial cs)
filterSpecial ('\\':cs) = "$\\backslash$"++(filterSpecial cs)
filterSpecial ('{':cs)  = '\\':'{':(filterSpecial cs)
filterSpecial ('}':cs)  = '\\':'}':(filterSpecial cs)
filterSpecial ('|':cs)  = "\\ensuremath{|}"++(filterSpecial cs)
filterSpecial ('<':'-':cs)  = "\\ensuremath{\\leftarrow}"++(filterSpecial cs)
filterSpecial ('<':cs)  = "\\ensuremath{\\langle}"++(filterSpecial cs)
filterSpecial ('-':'>':cs)  = "\\ensuremath{\\rightarrow}"++(filterSpecial cs)
filterSpecial ('>':cs)  = "\\ensuremath{\\rangle}"++(filterSpecial cs)
filterSpecial (c:cs)    = c:(filterSpecial cs)
filterSpecial []        = []


-- | Constructs the appropriate LaTeX macro for the given style.
latexHighlight :: Highlight -> (String, String)
latexHighlight Normal         = ("{\\rm{}", "}")
latexHighlight Bold           = ("{\\bf{}", "}")
latexHighlight Dim            = ("", "")
latexHighlight Underscore     = ("\\underline{", "}")
latexHighlight Blink          = ("", "")
latexHighlight ReverseVideo   = ("", "")
latexHighlight Concealed      = ("\\conceal{", "}")
latexHighlight (Foreground c) = ("\\textcolor{"++ latexColour c ++"}{", "}")
latexHighlight (Background c) = ("\\colorbox{"++ latexColour c ++"}{", "}")

-- | Translate a 'Colour' into a LaTeX colour name.
latexColour :: Colour -> String
latexColour Black   = "black"
latexColour Red     = "red"
latexColour Green   = "green"
latexColour Yellow  = "yellow"
latexColour Blue    = "blue"
latexColour Magenta = "magenta"
latexColour Cyan    = "cyan"
latexColour White   = "white"

-- | Generic LaTeX document preamble.
latexPrefix = unlines
    ["\\documentclass[a4paper, 12pt]{article}"
    ,"\\usepackage[usenames]{color}"
    ,"\\usepackage{hyperref}"
    ,"\\newsavebox{\\spaceb}"
    ,"\\newsavebox{\\tabb}"
    ,"\\savebox{\\spaceb}[1ex]{~}"
    ,"\\savebox{\\tabb}[4ex]{~}"
    ,"\\newcommand{\\hsspace}{\\usebox{\\spaceb}}"
    ,"\\newcommand{\\hstab}{\\usebox{\\tabb}}"
    ,"\\newcommand{\\conceal}[1]{}"
    ,"\\begin{document}"
    ,"\\noindent"
    ]

-- | Generic LaTeX document postamble.
latexSuffix = unlines
    [""
    ,"\\end{document}"
    ]
