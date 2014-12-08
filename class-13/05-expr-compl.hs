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

data Expr = Real Float | Complex Float Float | Bin Op Expr Expr
  deriving (Show, Eq)
data Op = Plus | Minus | Mul | Div
  deriving (Show, Eq)

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
    constant = (Real `liftM` float) <|> complex


-- Парсер вещественных чисел.
float :: Parser Float
float = do
  let
    posSum = foldl (\ acc n -> 10*acc + fromIntegral n) 0
    negSum = foldr (\ n acc -> 0.1*(acc + fromIntegral n)) 0
  sign <- optional True $ symbol "-" >> return False
  pos <- posSum `liftM` many1 digit
  neg <- (char '.' >> negSum `liftM` many1 digit) `mplus` return 0
  return $ if sign then pos + neg else -(pos + neg)

-- Комплексный парсер.
complex :: Parser Expr
complex = bracket "(" ")" $ sepByComma (token float) (token $ symbol ",")
  where
    sepByComma p sep = do
      a <- p
      sep
      b <- p
      return $ Complex a b


-- Тесты.

exprs :: [String]
exprs =
  [ "(4.5, 2.18)"
  , "4.5"
  , "(13.3, 1) * (-3.18)"
  , " (0, 56) / (2.1, -8) "
  , "((-1,1)*(-2 , 2) ) /(-3,3)"
  ]

parsed :: [Expr]
parsed =
  [ Complex 4.5 2.18
  , Real 4.5
  , Bin Mul (Complex 13.3 1.0) (Real (-3.18))
  , Bin Div (Complex 0.0 56.0) (Complex 2.1 (-8.0))
  , Bin Div (Bin Mul (Complex (-1.0) 1.0) (Complex (-2.0) 2.0))
      (Complex (-3.0) 3.0)
  ]


test :: Bool
test = parse expr `map` exprs == parsed
