{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances,
        FlexibleInstances, OverlappingInstances #-}

import Parser
import SimpleParsers
import ParseNumbers
import Control.Applicative hiding (many, optional)
import Control.Monad
import Data.List (sortBy)

{-
   Определите тип для многочлена с вещественными коэффициентами.
-}
type Poly = [Element]

data Element = El Float Int
  deriving (Show)

data Sign = Minus | Plus

eps = 0.000001 -- не смог без этого.

{-
  Реализуйте парсер для многочленов (примеры в файле poly.txt).
-}

poly :: Parser Poly
poly = do
  e1 <- element Plus
  es <- parseTail <|> (return [])
  return $ e1:es

parseTail :: Parser [Element]
parseTail = do
  e <- (operation >>= element)
  es <- parseTail <|> (return [])
  return $ e:es

operation :: Parser Sign
operation = (symbol "-" >> return Minus)
  <|> (symbol "+" >> return Plus)

element :: Sign -> Parser Element
element sign = do
  (coef, p) <- (,) <$> readCoef <*> (readPower <|> return 0)
    <|> (,) <$> return 1 <*> readPower
  return $ El (correct coef sign) p

readPower :: Parser Int
readPower = (symbol "x^" >> natural) <|> (symbol "x" >> return 1)

readCoef :: Parser Float
readCoef = token float <|> (bracket "(" ")" $ token float)

correct :: Float -> Sign -> Float
correct f s = case s of
  Minus -> (-f)
  Plus -> f

float :: Parser Float
float = do
  let
    posSum = foldl (\ acc n -> 10*acc + fromIntegral n) 0
    negSum = foldr (\ n acc -> 0.1*(acc + fromIntegral n)) 0
  sign <- operation <|> (return Plus)
  pos <- posSum `liftM` many1 digit
  neg <- (char '.' >> negSum `liftM` many1 digit) <|> return 0
  return $ correct (pos + neg) sign

{-
   Напишите функцию, которая вычисляет частное и остаток при делении многочлена на многочлен.
-}
divmod :: Poly -> Poly -> (Poly, Poly)
divmod = iterF
  where
    liftCon e (es, p) = (e:es, p)
    iterF p1 p2
      | p1 < p2 = ([], p1)
      | otherwise  = liftCon newQ $ iterF newD p2
      where
        newD = p1 `subPoly` (newQ `multPoly` p2)
        newQ = p1 `divPolyMax` p2

divPolyMax :: Poly -> Poly -> Element
divPolyMax es1 es2 = (maximum es1) `divEl` (maximum es2)
  where (El v1 p1) `divEl` (El v2 p2) = El (v1/v2) (p1 - p2)

multPoly :: Element -> Poly -> Poly
multPoly (El v p) = map (\ (El vc pc) -> El (v*vc) (p+pc))

subPoly :: Poly -> Poly -> Poly
subPoly p1 p2 = getRidOfZero $ zipPolyWith subElems id negElem p1 p2
  where
    getRidOfZero = sortPoly . filter (\ (El v _) -> abs v > eps)
    negElem (El v p) = El (-v) p
    subElems (El v1 p) (El v2 _) = El (v1 - v2) p

zipPolyWith :: (Element -> Element -> Element) -> (Element -> Element) ->
  (Element -> Element) -> Poly -> Poly -> Poly
zipPolyWith zF lF rF p1 p2 = iterF pes1 pes2
  where
    (pes1, pes2) = (sortPoly p1, sortPoly p2)
    iterF es1 [] = map lF es1
    iterF [] es2 = map rF es2
    iterF s1@(e1:es1) s2@(e2:es2)
      | e1 > e2   = lF e1 : iterF es1 s2
      | e1 < e2   = rF e2 : iterF s1 es2
      | otherwise = zF e1 e2 : iterF es1 es2

instance Eq Element where
  (El _ p1) == (El _ p2) = p1 == p2

instance Ord Element where
  (El _ p1) <= (El _ p2) = p1 <= p2

instance Ord Poly where
    es1 <= es2 = (maximum es1) <= (maximum es2)

sortPoly :: Poly -> Poly
sortPoly es = sortBy comp es
  where
    comp (El _ p1) (El _ p2)
      | p1 > p2 = LT
      | p1 < p2 = GT
      | otherwise = EQ

{-
   Напишите функцию, которая вычисляет наибольший общий делитель двух многочленов.
-}
poly_gcd :: Poly -> Poly -> Poly
poly_gcd = undefined

{-
   Напишите функцию, которая вычисляет наибольший общий делитель списка многочленов.
   Не забудьте воспользоваться свёрткой.
-}
poly_gcd_list :: [Poly] -> Poly
poly_gcd_list = foldl1 poly_gcd

{-
   Дан текстовый файл, в каждой строке которого записан один многочлен. Вычислите наибольший
   общий делитель многочленов из файла. Предусмотрите вывод соответствующего сообщения, если
   какая-либо строка файла имеет некорректный формат.
-}
poly_gcd_file :: FilePath -> IO (Either String Poly)
poly_gcd_file = undefined

{-
   В параметрах командной строки задано имя файла с многочленами. Найти их наибольший общий делитель.
   Предусмотреть корректную обработку ошибок (неправильное количество параметров командной строки,
   отсутствие файла, неверный формат файла и пр.).
-}
main = undefined


testingChains :: [String]
testingChains =
  [ "5x^3 -7x + 2"
  , "2x^6 +x^3  - 3x^2+ 1"
  , "x^2 - 3 + x"
  , "x^3 - 12x^2 - 42"
  , "x-3"
  ]

testPolyParsing :: [Poly]
testPolyParsing = (sortPoly . parse poly) `map` testingChains

testDivmod :: Bool
testDivmod = divmod p1 p2 == answer
  where
    p1 = (sortPoly . parse poly) "x^3 - 12x^2 - 42"
    p2 = (sortPoly . parse poly) "x-3"
    answer = ([El 1 2, El (-9) 1, El (-27) 0], [El (-123) 0])
