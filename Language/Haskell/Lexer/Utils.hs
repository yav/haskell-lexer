module Language.Haskell.Lexer.Utils
  ( module Language.Haskell.Lexer.Utils
  , Token(..)
  ) where

import Language.Haskell.Lexer.Tokens

gotEOF :: [a] -> [(Token,[a])]
gotEOF [] = []
gotEOF as = [(GotEOF, reverse as)]

gotError :: [a] -> [a] -> [(Token, [a])]
gotError as is =
  (ErrorToken, reverse as):
  if null is then [(GotEOF,[])] else [(TheRest,is{-reverse (take 80 is)-})]

-- Inlining the call to output does not make a big difference.
--output token as cont = (token, reverse as):cont

-- Not reversing the token string seems to save about 10% of the time with HBC.
-- The difference in speed seems insignificant with ghc-6.0.1 -O.

output :: t -> [a] -> [(t, [a])] -> [(t, [a])]
output token as cont = (token,reverse as):cont

-- This avoids constructing a closure for the call to reverse.
-- This saves about 10% too.
{-
output token as cont =
    rev as []
  where
    rev [] as' = (token,as'):cont
    rev (a:as) as' = rev as (a:as')
--}

nestedComment :: [Char] -> [Char] -> (([a] -> [a] -> [(Token, [a])])
              -> [Char] -> [Char] -> [(Token, [Char])]) -> [(Token, [Char])]

nestedComment as' is' next = nest (0::Int) as' is'
  where
    nest n as is =
      case is of
        '-' : '}' : is1   -> if n == 0
                               then next gotError ('}':'-':as) is1
                               else nest (n-1) ('}':'-':as) is1
        '{' : '-' : is1   -> nest (n+1) ('-':'{':as) is1
        c : is1           -> nest n (c:as) is1
        []                -> gotError as is -- EOF inside comment
