module Colourise
  ( module ColourHighlight
  , ColourPrefs
  , readColourPrefs
  , colourise
  ) where

import ColourHighlight
import System (getEnv)
import Char
import List

data ColourPrefs = ColourPrefs
  { keyword, keyglyph, layout, comment
  , conid, varid, conop, varop
  , string, char, number
  , selection, variantselection :: [Highlight]
  } deriving (Eq,Show,Read)

defaultColourPrefs = ColourPrefs
  { keyword  = [Foreground Green,Underscore]
  , keyglyph = [Foreground Red]
  , layout   = [Foreground Cyan]
  , comment  = [Foreground Blue]
  , conid    = [Normal]
  , varid    = [Normal]
  , conop    = [Foreground Red,Bold]
  , varop    = [Foreground Cyan]
  , string   = [Foreground Magenta]
  , char     = [Foreground Magenta]
  , number   = [Foreground Magenta]
  , selection = [Bold, Foreground Magenta]
  , variantselection = [Dim, Foreground Red, Underscore]
  }

readColourPrefs :: IO ColourPrefs
readColourPrefs = catch
  (do val <- readFile ".hscolour"
      return (read val))
  (\e-> catch
    (do home <- getEnv "HOME"
        val <- readFile (home++"/.hscolour")
        return (read val))
    (\e-> return defaultColourPrefs))

colourise :: ColourPrefs -> String -> [(String,[Highlight])]
colourise pref = map (\s->(s,classify pref s)) . glue . tokenise

-- Lex Haskell source code into a token stream
tokenise :: String -> [String]
tokenise []    = []
tokenise (c:s) | isSpace c
               = (c:ss): tokenise rest where (ss,rest) = span isSpace s
tokenise s     = tok: tokenise rest where (tok,rest) = head (Prelude.lex s)

-- Glue sequences of tokens into more useful blobs
--glue (q:".":n:rest) | Char.isUpper (head q)	-- qualified names
--                    = glue ((q++"."++n): rest)
glue ("`":rest) =				-- `varid` -> varop
  case glue rest of
    (qn:"`":rest) -> ("`"++qn++"`"): glue rest
    _             -> ("`": rest)
glue (s:ss)       | all (=='-') s		-- eol comment
                  = (s++concat c): glue rest
                  where (c,rest) = break ('\n'`elem`) ss
glue ("{":"-":ss)  = ("{-"++c): glue rest	-- nested comment
                  where (c,rest) = nestcomment 0 ss
glue (s:ss)       = s: glue ss
glue []           = []

nestcomment :: Int -> [String] -> (String,[String])
nestcomment n ("{":"-":ss) | n>=0 = (("{-"++cs),rm)
                                  where (cs,rm) = nestcomment (n+1) ss
nestcomment n ("-":"}":ss) | n>0  = (("-}"++cs),rm)
                                  where (cs,rm) = nestcomment (n-1) ss
nestcomment n ("-":"}":ss) | n==0 = ("-}",ss)
nestcomment n (s:ss)       | n>=0 = ((s++cs),rm)
                                  where (cs,rm) = nestcomment n ss
nestcomment n [] = error "no closing comment -}"

{-
data TokenType =
  KeyWord | KeyGlyph | Layout | Comment | ConId | VarId | ConOp | VarOp |
  String  | Char | Number | Error
  deriving (Eq)
-}

-- Classify tokens
classify :: ColourPrefs -> String -> [Highlight]
classify pref s@(h:_)
    | isSpace h              = [Normal]
    | all (=='-') s          = comment pref
    | "--" `isPrefixOf` s
      && any isSpace s       = comment pref	-- not fully correct
    | "{-" `isPrefixOf` s    = comment pref
    | s `elem` keywords      = keyword pref
    | s `elem` keyglyphs     = keyglyph pref
    | s `elem` layoutchars   = layout pref
    | isUpper h              = conid pref
    | isLower h              = varid pref
    | h `elem` symbols       = varop pref
    | h==':'                 = conop pref
    | h=='`'                 = varop pref
    | h=='"'                 = string pref
    | h=='\''                = char pref
    | isDigit h              = number pref
    | otherwise              = selection pref

-- Haskell keywords
keywords =
  ["case","class","data","default","deriving","do","else"
  ,"if","import","in","infix","infixl","infixr","instance","let","module"
  ,"newtype","of","then","type","where","_","foreign","ccall","as"]
keyglyphs =
  ["..","::","=","\\","|","<-","->","@","~","=>","[","]"]
layoutchars =
  map (:[]) ";{}(),"
symbols =
  "!#$%&*+./<=>?@\\^|-~"
