module Language.Haskell.Lexer.Tokens where

-- | Haskell token classifications:
data Token
  = Varid       -- ^ Variable
  | Conid       -- ^ Constructor
  | Varsym      -- ^ Variable operator
  | Consym      -- ^ Constructor operator
  | Reservedid  -- ^ Reserved keyword
  | Reservedop  -- ^ Reserved operator
  | Specialid
  | IntLit      -- ^ Integral numeric literal
  | FloatLit    -- ^ Fractional numeric literal
  | CharLit     -- ^ Character literal
  | StringLit   -- ^ String literal


  | QQuote      -- ^ Quasi quote: @[|text|stuff|]@

  | Qvarid      -- ^ Qualified variable
  | Qconid      -- ^ Qualified constructor
  | Qvarsym     -- ^ Qualified variable operator
  | Qconsym     -- ^ Qualified constructor operator

  | Special
  | Whitespace  -- ^ White space

  | NestedCommentStart  -- ^ Internal: causes a call to an external function
  | NestedComment       -- ^ A nested comment ({- ... -})
  | LiterateComment     -- ^ Not handled by the lexer

  | Commentstart        -- ^ Dashes
  | Comment             -- ^ The stuff after the dashes

  | ErrorToken | GotEOF | TheRest

  | ModuleName | ModuleAlias -- ^ recognized in a later pass

  -- Inserted during layout processing (see Haskell 98, 9.3):
  | Layout     -- ^ for implicit braces
  | Indent Int -- ^ \<n\>, to preceed first token on each line
  | Open Int   -- ^ \{n\}, after let, where, do or of, if not followed by a \"{\"
  deriving (Show,Eq,Ord)
