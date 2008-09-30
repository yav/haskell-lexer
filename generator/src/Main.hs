-- This program generates the core of lexical analyser for Haskell

import Spec.HaskellLexicalSyntax(program) -- The lexical syntax specification
import LexerGen(lexerGen)                 -- The lexer generator implementation
import System.Environment(getArgs)

main = putStrLn . lexerGen "HsLex" "haskellLex" program =<< getArgs
