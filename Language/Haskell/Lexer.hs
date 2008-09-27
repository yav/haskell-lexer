module Language.Haskell.Lexer
  ( PosToken
  , Token(..)
  , lexerPass0
  , lexerPass0'
  , lexerPass1
  , rmSpace
  , layoutPre
  , module Language.Haskell.Lexer.Position
  ) where

import Language.Haskell.Lexer.Lex(haskellLex)
import Language.Haskell.Lexer.Utils
import Language.Haskell.Lexer.Layout(layoutPre,PosToken)
import Language.Haskell.Lexer.Position
import Data.List(mapAccumL)

default(Int)

-- | The function 'lexerPass1' handles the part of lexical analysis that
-- can be done independently of the parser---the tokenization and the
-- addition of the extra layout tokens \<n\> and {n}, as specified in
-- section 9.3 of the revised Haskell 98 Report.
lexerPass1 :: String -> [PosToken]
lexerPass1 = lexerPass1Only . lexerPass0

lexerPass1Only :: [PosToken] -> [PosToken]
lexerPass1Only = layoutPre . rmSpace

-- | Remove token that are not meaningful (e.g., white space and comments).
rmSpace :: [PosToken] -> [PosToken]
rmSpace = filter (notWhite.fst)

notWhite :: Token -> Bool
notWhite t = t/=Whitespace &&
             t/=Commentstart && t/=Comment &&
             t/=NestedComment

-- | Tokenize and add position information.  Preserves white space,
-- and does not insert extra tokens due to layout.
lexerPass0 :: String -> [PosToken]
lexerPass0 = lexerPass0' startPos

-- | Same as 'lexerPass0', except that it uses the given start position.
lexerPass0' :: Pos -> String -> [PosToken]
lexerPass0' pos0 = addPos . haskellLex . rmcr
  where
    addPos = snd . mapAccumL pos pos0

    pos p (t,s) = {-seq p'-} (p',(t,(p,s)))
        where p' = nextPos p s
--      where s = reverse r


-- | Since #nextPos# examines one character at a time, it will increase the line
-- number by 2 if it sees \CR\LF, which can happen when reading DOS files on
-- a Unix like system.
-- Since the extra \CR characters can cause trouble later as well, we choose
-- to simply remove them here.
rmcr :: String -> String
rmcr ('\CR':'\LF':s) = '\LF':rmcr s
rmcr (c:s) = c:rmcr s
rmcr "" = ""

