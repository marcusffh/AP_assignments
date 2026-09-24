module APL.Parser (parseAPL) where

import APL.AST (Exp (..), VName)
import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Void (Void)
import Text.Megaparsec
  ( Parsec,
    choice,
    chunk,
    eof,
    errorBundlePretty,
    many,
    notFollowedBy,
    parse,
    satisfy,
    some,
    try,
  )
import Text.Megaparsec.Char (space)

type Parser = Parsec Void String

lexeme :: Parser a -> Parser a
lexeme p = p <* space

keywords :: [String]
keywords =
  [ "if",
    "then",
    "else",
    "true",
    "false",
    "print", -- part 3
    "put", -- part 3
    "get", -- part 3
    "try", -- part 4
    "catch", -- part 4
    "let", -- part 4
    "in", -- part 4
    "loop", -- part 4
    "for", -- part 4
    "do" -- part 4
  ]

lVName :: Parser VName
lVName = lexeme $ try $ do
  c <- satisfy isAlpha
  cs <- many $ satisfy isAlphaNum
  let v = c : cs
  if v `elem` keywords
    then fail "Unexpected keyword"
    else pure v

lInteger :: Parser Integer
lInteger =
  lexeme $ read <$> some (satisfy isDigit) <* notFollowedBy (satisfy isAlphaNum)

lString :: String -> Parser ()
lString s = lexeme $ void $ chunk s

----- Part 3 below
pString :: Parser String
pString =
  lexeme $
    chunk "\"" *>
    many (satisfy (/= '"')) <*
    chunk "\""
--- Part 3 above

lKeyword :: String -> Parser ()
lKeyword s = lexeme $ void $ try $ chunk s <* notFollowedBy (satisfy isAlphaNum)

pBool :: Parser Bool
pBool =
  choice $
    [ const True <$> lKeyword "true",
      const False <$> lKeyword "false"
    ]

pAtom :: Parser Exp
pAtom =
  choice
    [ CstInt <$> lInteger,
      CstBool <$> pBool,
      Var <$> lVName,
      lString "(" *> pExp <* lString ")"
    ]

------------------------------------- part 1 below
pFExp :: Parser Exp
pFExp = pAtom >>= chain
  where
    chain x =
      choice
        [ do
            y <- pAtom
            chain $ Apply x y,
          pure x
        ]
-------------------------------- part 1 above
---------------------------------Part 3 below
pLExp :: Parser Exp
pLExp =
  choice
    [ try $
        If
          <$> (lKeyword "if" *> pExp)
          <*> (lKeyword "then" *> pExp)
          <*> (lKeyword "else" *> pExp),

      Print
        <$> (lKeyword "print" *> pString)
        <*> pAtom,

      KvGet
        <$> (lKeyword "get" *> pAtom),

      KvPut
        <$> (lKeyword "put" *> pAtom)
        <*> pAtom,
      
      Lambda 
        <$> (lString "\\" *> lVName)
        <*> (lString "->" *> pExp),

      Let
        <$> (lKeyword "let" *> lVName)
        <*> (lString "=" *> pExp)
        <*> (lKeyword "in" *> pExp),
      
      TryCatch
        <$> (lKeyword "try" *> pExp)
        <*> (lKeyword "catch" *> pExp),

      ForLoop
        <$> ((,) <$> (lKeyword "loop" *> lVName)
                <*> (lString "=" *> pExp))
        <*> ((,) <$> (lKeyword "for" *> lVName)
                <*> (lString "<" *> pExp))
        <*> (lString "do" *> pExp),

      pFExp
    ]
----------------- Part 3 above
-------------------------- Part 2 below
pExp2 :: Parser Exp
pExp2 = do
  x <- pLExp
  choice
    [ do
        lString "**"
        y <- pExp2
        pure $ Pow x y,
      pure x
    ]
----------------------------- Part 2 above

pExp1 :: Parser Exp
pExp1 = pExp2 >>= chain
  where
    chain x =
      choice
        [ do
            lString "*"
            y <- pExp2 -------- Part 2
            chain $ Mul x y,
          do
            lString "/"
            y <- pExp2 -------- Part 2
            chain $ Div x y,
          pure x
        ]

pExp0 :: Parser Exp
pExp0 = pExp1 >>= chain
  where
    chain x =
      choice
        [ do
            lString "+"
            y <- pExp1
            chain $ Add x y,
          do
            lString "-"
            y <- pExp1
            chain $ Sub x y,
          pure x
        ]

--------------- Part 2 below
pExp :: Parser Exp
pExp = pExp0 >>= chain
  where
    chain x =
      choice
        [ do
            lString "=="
            y <- pExp0
            chain $ Eql x y,
          pure x
        ]
---------------- Part 2 above

parseAPL :: FilePath -> String -> Either String Exp
parseAPL fname s = case parse (space *> pExp <* eof) fname s of
  Left err -> Left $ errorBundlePretty err
  Right x -> Right x
