module Anchors
  ( insertAnchors
  ) where

import Classify
import List

type Anchor = String

insertAnchors :: [(TokenType,String)] -> [Either Anchor (TokenType,String)]
insertAnchors = anchor emptyST

-- only looks at first token in the left-most position of each line
anchor st t@((Varid,v):stream)
--	| typesig stream = emit st t	-- only interested in definitions
	| v `inST` st    = emit st t	-- check not already defined
	| otherwise =
		let st' = insertST v st in
		Left v: emit st' t
anchor st t@((Layout,"("):stream) =
	-- munch tokens until past closing paren, then check for Varop
	emit st t	--dummy, not yet implemented
anchor st t@((Keyword,"foreign"):stream) =
	-- find identifier
	emit st t	--dummy, not yet implemented
anchor st t@((Keyword,"data"):stream) =
	-- skip possible context up to and past "=>"
	-- then check for Conid
	emit st t	--dummy, not yet implemented
anchor st t@((Keyword,"class"):stream) =
	-- skip possible context up to and past "=>"
	-- then check for Conid
	emit st t	--dummy, not yet implemented
anchor st stream = emit st stream

-- emit passes stuff through until the next newline has been encountered,
-- then jumps back into the anchor function
-- pre-condition: newlines are explicitly single tokens
emit st (t@(Space,"\n"):stream) = Right t: anchor st stream
emit st (t:stream)              = Right t: emit st stream
emit _  []                      = []

-- simple implementation of a string lookup table.
-- replace this with something more sophisticated if needed.
type ST = [String]

emptyST :: ST
emptyST = []

insertST :: String -> ST -> ST
insertST k st = insert k st

inST :: String -> ST -> Bool
inST k st = k `elem` st
