import Parser
import SimpleParsers
import ParseNumbers
import Control.Monad

{- Напишите парсер для вещественных чисел. -}

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

{-
  Напишите парсер для представления комплексных чисел,
  записываемых в виде вещественной и мнимой части через запятую
  в круглых скобках, например, "(2.3, 1)".

-}
complex :: Parser (Float, Float)
complex = bracket "(" ")" $ sepByComma (token float) (symbol ",")
  where sepByComma p sep = p >>= \ a -> sep >> p >>= \ b -> return (a, b)

{-
  Напишите парсер для списка комплексных чисел (разделитель — точка с запятой),
  заключённого в квадратные скобки.
-}
complexList :: Parser [(Float, Float)]
complexList = bracket "[" "]" $ sepBy (token complex) (symbol ";")

{-
  Модифицируйте предыдущий парсер таким образом, чтобы в исходной строке
  могли встречаться как комплексные числа, так и вещественные (мнимая часть
  при этом должна считаться равной нулю).
-}
complexList2 :: Parser [(Float, Float)]
complexList2 = bracket "[" "]" $ sepBy tok (symbol ";")
  where tok = token complex `mplus` (token float >>= \ a -> return (a,0))

{-
   Модифицируйте предыдущий парсер таким образом, чтобы компоненты списка
   разделялись запятой, а не точкой запятой. Постарайтесь реализовать
   требуемое с помощью вспомогательных парсеров, допускающих повторное применение.
-}
complexList3 :: Parser [(Float, Float)]
complexList3 = bracket "[" "]" $ sepBy tok (symbol ",")
  where tok = token complex `mplus` (token float >>= \ a -> return (a,0))



-- Тесты.

testFloat :: Bool
testFloat =  posCheck && negCheck
  where
    posCheck = (parse float) `map` posTest == posAns
    posTest = ["4.6", "-0.35", "2.41", "5.", "-.2"]
    posAns = [4.6, -0.35, 2.41, 5.0, -0.2]
    negTest = ["4.-6", "3", "-asdas", "bcd", "5.6b", "5.b"]
    negAns = [(4.0,"-6"), (5.6,"b"), (5.0,"b")]
    negCheck = (apply float) `concatMap` negTest == negAns

testComplex :: Bool
testComplex = posCheck && negCheck
  where
    posCheck = (parse complex) `map` posTest == posAns
    posTest = ["(4.6, -0.35)", " ( 2.41, 5.)", "(-.2 , -4.19)"]
    posAns = [(4.6,-0.35), (2.41, 5.0), (-0.2,-4.19)]
    negTest = ["(4.-6,-asdas)", "bcd", "(5.6b)", "(5.b, 4.19)"]
    negCheck = (apply float) `concatMap` negTest == []

testLists :: Bool
testLists = posCheck && negCheck
  where
    posCheck = parse complexList posTest == posAns
    posTest = "[(4.6, -0.35); ( 2.41, 5.) ;(-.2 , -4.19)]"
    posAns = [(4.6,-0.35), (2.41, 5.0), (-0.2,-4.19)]
    negTest = ["[(1,-1),(2,4)]", "[();()]", "[(5.6);(3,4)]", "[4.5]"]
    negCheck = (apply float) `concatMap` negTest == []

testLists2 :: Bool
testLists2 = posCheck
  where
    posCheck = parse complexList2 posTest == posAns
    posTest = "[4.5 ;(-.2 , -4.19); 6.7 ; (1.,1.)]"
    posAns = [(4.5,0),(-0.2,-4.19),(6.7,0), (1,1)]

testLists3 :: Bool
testLists3 = posCheck
  where
    posCheck = parse complexList3 posTest == posAns
    posTest = "[4.5 ,(-.2 , -4.19), 6.7 , (1.,1.)]"
    posAns = [(4.5,0),(-0.2,-4.19),(6.7,0), (1,1)]
