-- This program generates the core of lexical analyser for Haskell

import Spec.HaskellLexicalSyntax(program) -- The lexical syntax specification
import LexerGen(lexerGen)                 -- The lexer generator implementation
import System.Environment(getArgs)

main = putStrLn . lexerGen "Language.Haskell.Lexer.Lex" "haskellLex" program =<< getArgs
