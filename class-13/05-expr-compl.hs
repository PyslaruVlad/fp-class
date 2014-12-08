import Parser
import SimpleParsers
import ParseNumbers
import Control.Applicative hiding (many, optional)
import Control.Monad

{-
   Добавьте поддержку вещественных и комплексных чисел из второго упражнения.
   Можете считать, что все числа в выражении являются комплексными (и можете
   не считать, если в состоянии красиво обойтись с типами и всё корректно
   проанализировать).
-}

data Expr = Con Float | Bin Op Expr Expr
  deriving Show
data Op = Plus | Minus | Mul | Div
  deriving Show

{-
expr   ::= term {addop term}*
term   ::= factor {mulop factor}*
factor ::= nat | '(' expr ')'
addop  ::= '+' | '-'
mulop  ::= '*' | '/'
-}

expr :: Parser Expr
expr = token (term >>= rest addop term)
  where
    rest op unit e1 = optional e1 $ do
        p <- op
        e2 <- unit
        rest op unit $ Bin p e1 e2
    term = token (factor >>= rest mulop factor)
    factor = token (constant <|> bracket "(" ")" expr)
    addop = binop ("+", Plus) ("-", Minus)
    mulop = binop ("*", Mul) ("/", Div)
    binop (s1, cons1) (s2, cons2) =
          (symbol s1 >> return cons1) <|>
          (symbol s2 >> return cons2)
    constant = Con `liftM` float


-- Решение.

-- Из второго задания.
float :: Parser Float
float = do
  let
    posSum = foldl (\ acc n -> 10*acc + fromIntegral n) 0
    negSum = foldr (\ n acc -> 0.1*(acc + fromIntegral n)) 0
  sign <- optional True $ symbol "-" >> return False
  pos <- posSum `liftM` many digit
  char '.'
  neg <- negSum `liftM` many digit
  return $ if sign then pos + neg else -(pos + neg)

-- Данные для представления комплексных чисел.
data CompExpr = Real Expr | Complex (Expr, Expr) | CBin Op CompExpr CompExpr
  deriving Show

-- Парсер.
complex :: Parser CompExpr
complex = simple <|> complex
  where
    complex = bracket "(" ")" $ sepByComma (token expr) (symbol ",")
    simple = Real <$> expr
    sepByComma p sep = do
      a <- p
      sep
      b <- p
      return $ Complex (a, b)



-- Тесты.

exprs :: [String]
exprs =
  [ "(4.5 * (-0.2) , 2.18 - 0.2)"
  , "4.5"
  , "(13./3., (-0.15) * 1.)"
  ]

test :: [CompExpr]
test = parse complex `map` exprs
