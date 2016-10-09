module Language.Haskell.Lexer.Position where

-- | The posisiotn within a file.
data Pos = Pos { char, line, column :: !Int } deriving (Show)

-- | The line and column numbers of a position.
simpPos :: Pos -> (Int,Int)
simpPos (Pos _ l c) = (l,c)

-- Some functions still put fake char positions in Pos values, so...
instance Eq Pos where p1 == p2 = simpPos p1 == simpPos p2
instance Ord Pos where compare p1 p2 = compare (simpPos p1) (simpPos p2)

-- | The first column is designated column 1, not 0.
startPos :: Pos
startPos = Pos { char = 0, line = 1, column = 1 }

-- | Advance position by a string.
nextPos :: Pos -> String -> Pos
nextPos = foldl nextPos1

-- | Advance position by a single character.
nextPos1 :: Pos -> Char -> Pos
nextPos1 (Pos n y x) c =
    case c of
      -- The characters newline, return, linefeed, and formfeed, all start
      -- a new line.
      '\CR' -> Pos (n+1) (y+1) 1
      '\LF' -> Pos (n+1) (y+1) 1
      '\FF' -> Pos (n+1) (y+1) 1
      -- Tab stops are 8 characters apart.
      -- A tab character causes the insertion of enough spaces to align the
      -- current position with the next tab stop.
      -- + (not in the report) the first tab stop is column 1.
      '\t'  -> Pos (n+1) y (x+8-(x-1) `mod` 8)
      _ -> Pos (n+1) y (x+1)
