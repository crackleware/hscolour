module Language.Haskell.HsColour.Options
  ( Option(..)
  , Output(..)
  , Literate(..)
  ) where 

import Language.Haskell.HsColour.Output

-- | Command-line options
data Option =
    Help		-- ^ print usage message
  | Version		-- ^ report version
  | Information		-- ^ report auxiliary information, e.g. CSS defaults
  | Format Output	-- ^ what type of output to produce
  | LHS Literate	-- ^ literate input (i.e. multiple embedded fragments)
  | Anchors Bool	-- ^ whether to add anchors
  | Partial Bool	-- ^ whether to produce a full document or partial
  | Input FilePath	-- ^ input source file
  | Output FilePath	-- ^ output source file
  deriving Eq

data Literate = NoLit | Bird | TeX deriving Eq
