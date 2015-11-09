module MonaParser where

import Logic

parseFile :: FilePath -> Formula
parseFile filename = error "Unimplemented"

data Token
  = Exists1
  | Exists2
  deriving (Read, Show, Enum, Eq, Ord)


-- tokenizes a string
tokenize :: String -> [Token]
tokenize str = error "Unimplemented"
