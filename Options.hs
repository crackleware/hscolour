module Options where 

import Language.Haskell.Output
-- | Command-line options
data Option =
    Help		-- ^ print usage message
  | Version		-- ^ report version
  | Information		-- ^ report auxiliary information, e.g. CSS defaults
  | Format Output	-- ^ what type of output to produce
  | LHS Bool		-- ^ literate input (i.e. multiple embedded fragments), only constructor used is LHS False in the program
  | Bird                -- ^ tag this is Bird literate style input, both using Bird and TeX is an error
  | TeX                 -- ^ tag to indicate we are using \begin{code} \end{code} literate style
  | Anchors Bool	-- ^ whether to add anchors
  | Partial Bool	-- ^ whether to produce a full document or partial
  | Input FilePath	-- ^ input source file
  | Output FilePath	-- ^ output source file
  deriving Eq
