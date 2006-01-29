module Anchors
  ( insertAnchors
  ) where

import Classify
import List

-- This is an attempt to find the first defining occurrence of an
-- identifier (function, datatype, class) in a Haskell source file.
-- Rather than parse the module properly, we try to get by with just
-- a finite state automaton.  Keeping a record of identifiers we
-- have already seen, we look at the beginning of every line to see
-- if it starts with the right tokens to introduce a defn.  If so,
-- we look a little bit further until we can be certain.  Then plonk
-- (or not) an anchor at the beginning of the line.

type Anchor = String

insertAnchors :: [(TokenType,String)] -> [Either Anchor (TokenType,String)]
insertAnchors = anchor emptyST

-- looks at first token in the left-most position of each line
anchor st t@((Varid,v):stream) =
    case skip stream of
        ((Varop,v):_) | not (v`inST`st) -> Left (fix v): emit (insertST v st) t
        notVarop      | typesig stream  -> emit st t  -- not a defn
                      | v `inST` st     -> emit st t  -- already defined
                      | otherwise       -> Left v: emit (insertST v st) t
anchor st t@((Layout,"("):stream) =
    case stream of
      ((Varop,v):(Layout,")"):_)
                      | typesig stream  -> emit st t
	              | v `inST` st     -> emit st t
	              | otherwise	-> Left (fix v): emit (insertST v st) t
      notVarop -> case skip (munchParens stream) of
          ((Varop,v):_) | not (v`inST`st) -> Left (fix v): emit (insert v st) t
          _             -> emit st t
anchor st t@((Keyword,"foreign"):stream) =
	-- find identifier
	emit st t	--dummy, not yet implemented
anchor st t@((Keyword,"data"):stream) =
	-- skip possible context up to and past "=>"
	-- then check for Conid
        getConid stream $ emit st t
anchor st t@((Keyword,"type"):stream) =
	getConid stream $ emit st t
anchor st t@((Keyword,"class"):stream) =
	getConid stream $ emit st t
anchor st stream = emit st stream

-- emit passes stuff through until the next newline has been encountered,
-- then jumps back into the anchor function
-- pre-condition: newlines are explicitly single tokens
emit st (t@(Space,"\n"):stream) = Right t: anchor st stream
emit st (t:stream)              = Right t: emit st stream
emit _  []                      = []

-- Is this really a type signature?
typesig :: [(TokenType,String)] -> Bool
typesig ((Keyglyph,"::"):_)   = True
typesig ((Varid,_):stream)    = typesig stream
typesig ((Layout,"("):(Varop,_):(Layout,")"):stream)    = typesig stream
typesig ((Layout,","):stream) = typesig stream
typesig ((Space,_):stream)    = typesig stream
typesig ((Comment,_):stream)  = typesig stream
typesig _                     = False

-- throw away everything from opening paren to matching close
munchParens =  munch 0	-- already seen open paren
  where munch 0 ((Layout,")"):rest) = rest
        munch n ((Layout,")"):rest) = munch (n-1) rest
        munch n ((Layout,"("):rest) = munch (n+1) rest
        munch n (_:rest)            = munch n rest
        munch _ []                  = []	-- source is ill-formed

-- ensure anchor name is correct for a Varop
fix ('`':v) = init v
fix v       = v

-- look past whitespace and comments to next "real" token
skip ((Space,_):stream)   = skip stream
skip ((Comment,_):stream) = skip stream
skip stream               = stream

-- munch past possible context, returning next Conid token
-- (this function is highly partial - relies on source being parse-correct)
getConid stream =
    case skip stream of
        ((Conid,c):rest) -> case context rest of
                              ((Keyglyph,"="):_)     -> (:) (Left c)
                              ((Keyglyph,"=>"):more) ->
                                  case skip more of
                                      ((Conid,c'):_) -> (:) (Left c')
        ((Layout,"("):rest) -> case context rest of
                                   ((Keyglyph,"=>"):more) ->
                                       case skip more of
                                           ((Conid,c'):_) -> (:) (Left c')
--  where debug (s:t) = error ("getConid failed: "++show s)

-- jump past possible class context
context stream@((Keyglyph,"="):_) = stream
context stream@((Keyglyph,"=>"):_) = stream
context (_:stream) = stream

-- simple implementation of a string lookup table.
-- replace this with something more sophisticated if needed.
type ST = [String]

emptyST :: ST
emptyST = []

insertST :: String -> ST -> ST
insertST k st = insert k st

inST :: String -> ST -> Bool
inST k st = k `elem` st
