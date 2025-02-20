{-|
This module collects the definitions from the Lexical Syntax in appendix B.3
of the (revised) Haskell 98 report that define sets of characters.
These sets are referred to in the rest of the lexical syntax,
which is given in module HaskellLexicalSyntax.

A deviation is the more relaxed handling of certain unicode characters as in
GHC, see https://downloads.haskell.org/ghc/9.12.1/docs/users_guide/bugs.html for
details.
-}

module Spec.HaskellChars where
import Data.Char

{-|
ASCII characters are represented by themselves, while
non-ASCII characters are represented by the class they belong to.
-}
data HaskellChar
  = ASCII Char
  | UniWhite   -- any Unicode character defined as whitespace
  | UniSymbol  -- any Unicode symbol or punctuation
  | UniDigit   -- any Unicode Decimal Number, Letter Number or Other Number
  | UniLarge   -- any uppercase or titlecase Unicode letter
  | UniSmall   -- any Unicode Lowercase Letter or Other Letter
  | UniIdchar  -- any Unicode Modifier Letter or Non-Spacing Mark
  deriving (Eq,Ord{-,Show-})

acs = map ASCII

-- Character classifications:
special   = acs "(),;[]`{}"
creturn   = acs "\r"
linefeed  = acs "\LF"
vertab    = acs "\VT"
formfeed  = acs "\FF"
space     = acs " \xa0"
tab       = acs "\t"
uniWhite  = [UniWhite]
cany      = graphic++space++tab++uniIdchar
graphic   = small++large++symbol++digit++special++acs ":\"'"
small     = ascSmall++uniSmall++acs "_"
ascSmall  = acs ['a'..'z']
uniSmall  = [UniSmall]
large     = ascLarge++uniLarge
ascLarge  = acs ['A'..'Z']
uniLarge  = [UniLarge]
symbol    = ascSymbol++uniSymbol
ascSymbol = acs "!#$%&*+./<=>?@\\^|-~"
uniSymbol = [UniSymbol]
digit     = ascDigit++uniDigit
ascDigit  = acs ['0'..'9']
uniDigit  = [UniDigit]
octit     = acs ['0'..'7']
hexit     = digit ++ acs ['A'..'F'] ++ acs ['a'..'f']
uniIdchar = [UniIdchar]

----

instance Show HaskellChar where
  showsPrec d h =
    case h of
      ASCII c   -> showsPrec d c
      UniWhite  -> showString "UW"
      UniSymbol -> showString "US"
      UniDigit  -> showString "UD"
      UniLarge  -> showString "UU"
      UniSmall  -> showString "UL"

  showList s = showChar '"' . foldr ((.).shC) id s . showChar '"'
    where
      shC h =
        case h of
          ASCII c   -> showString (init . tail $ show c)
          UniWhite  -> showS "UW"
          UniSymbol -> showS "US"
          UniDigit  -> showS "UD"
          UniLarge  -> showS "UU"
          UniSmall  -> showS "UL"
        where showS s = showChar '"' . showString s . showChar '"'
