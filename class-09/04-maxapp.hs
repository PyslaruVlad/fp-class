{-# Language FlexibleInstances #-}
import Control.Applicative
import System.Random
import Data.Maybe

{-
  Пользуясь возможностями аппликативных функторов, определите функцию, 
  вычисляющую наибольший из результатов двух вычислений (значений в некотором
  контексте), принимаемых в качестве параметров (для результатов имеется
  экземпляр класса типов Ord).
-}

maxApp2 :: (Ord a, Applicative f) => f a -> f a -> f a
maxApp2 v1 v2 = max <$> v1 <*> v2

{- Реализуйте аналогичную функцию в случае трёх заданных значений в контексте. -}

maxApp3 :: (Ord a, Applicative f) => f a -> f a -> f a -> f a
maxApp3 v1 v2 v3 = maxApp2 v3 $ maxApp2 v1 v2

{- Реализуйте аналогичную функцию в случае списка значений в контексте. -}

maxApp :: (Ord a, Applicative f) => [f a] -> f a
maxApp = foldl1 maxApp2

{-
  Продемонстрируйте использование написанных функций для аппликативных функторов Maybe,
  список (для каждого из двух экземпляров), Either String и IO.
-}

{-
    Я подумал так: все равно корректность maxApp базируется на корректности maxApp2.
    И корректность maxApp3 опирается на maxApp2. Поэтому решил проверять maxApp
    надеясь на некую транзитивность.
-}

-- Создание контекста над случайными числами.
makeRandom :: Int -> (Int -> b) -> IO ([b], [Int]) 
makeRandom c f = newStdGen >>= return . getAns . (take c) . randomRs (0,100)
    where
        getAns ls = (map f ls, ls)

-- Вывод результата теста.
testResult :: Int -> Bool -> IO ()
testResult i condition = do
    if condition 
        then putStrLn $ "Test "++(show i)++" have been passed."
        else putStrLn $ "Didn't pass test "++(show i)++"."
    
-- Тестируем аппликативность только с одним конструктором
-- (без нейтрального элемента)
testPositive :: (Applicative f, Ord a) => Int -> Int -> (Int -> f a) -> 
    (f a -> Int) -> IO ()
testPositive i iter cons decons = do
    (rndS, rndN) <- makeRandom iter cons
    let
        rndMax = maximum rndN
        rndSomeMax = maxApp rndS
    testResult i $ rndMax == decons rndSomeMax

-- Вставляем нейтральный элемент.
testNegative :: (Applicative f, Eq (f a), Ord a) => Int ->  Int -> 
    (Int -> f a) -> f a -> IO ()
testNegative i iter cons neutral = do
    (rnd1, _) <- makeRandom iter cons
    (rnd2, _) <- makeRandom iter cons
    let
        args = rnd1++[neutral]++rnd2
        someMax = maxApp args
    testResult i $ someMax == neutral



test :: Int -> IO ()
-- Тесты для Maybe.
test 1 = testPositive 1 100 Just fromJust
test 2 = testNegative 2 25 Just Nothing
-- Тесты для Either.
test 3 = testPositive 3 100 Right (\(Right r) -> r)
test 4 = testNegative 4 25 Right (Left "abc")
-- Тесты для списка. Нулевой вариант.
test 5 = testPositive 5 100 (:[]) (\[x] -> x)
test 6 = testNegative 6 25 (:[]) []
-- Первый вариант. Оказалось невозможным проверить с предыдущими числами итераций.
-- Ибо например testPos... 100 (replicate 2) ... увеличивает длину конечного списка
-- в 2^100 раз (кажется). И в итоге программа не завершается. Число итераций поэтому меньше.
test 7 = testPositive 7 15 (replicate 2) (\ xs -> minimum xs)
-- минимум как бы говорит: "если где-то не максимум, то он будет использован для сравнения",
-- что в итоге должно показать неправильность функции.
test 8 = testNegative 8 20 (replicate 2) []
-- Второй тест на списках:
test 9 = testPositive 9 100 (ZipList . (replicate 2)) (minimum . getZipList)
test 10 = testNegative 10 25 (ZipList . (replicate 2)) (ZipList [])
-- Тест для IO. Кривовато, ибо нельзя очиситься от IO.
test 11 = do
    (rndIO, rndN) <- makeRandom 100 return
    let
        rndMax = maximum rndN
    rndIOMax <- maxApp rndIO
    testResult 11 $ rndMax == rndIOMax

-- Небольшое дополнение для 10ого теста.
instance Eq a => Eq (ZipList a) where
    ZipList aList == ZipList bList = aList == bList


main = mapM_ test [1..11]

{- (необязательно)
  Ясно ли вам, что вы реализовали нечто, похожее на моноид на аппликативных функторах?
  Можете ли вы выразить это в коде? Необходимо ли добавлять какие-нибудь ограничения?
-}

{-
    Не нашел другого способа, как переписать свою версию моноида.
    Потому что в противном случае возникают неоднозначности и перекрытия.
    Например, если определить наследование вида
    instance (Applicative f, Ord a, Bounded a) => Data.Monoid (f a) where
        ...
    То mappend [1] [2] будет давать результат вида [1,2] а не [2], ибо комплитор 
    считает, что тот экземпляр который я определяю здесь и сейчас не православен, а
    нужно использовать аналогичный из Data.Monoid. Перепробовал различные флаги -
    не помогло.
-}

class AppMonoid m where
    mappend :: m -> m -> m
    mconcat :: [m] -> m
    mconcat = foldl1 mappend

instance (Applicative f, Ord a) => AppMonoid (f a) where
    mappend = maxApp2

performMonoidTest :: IO ()
performMonoidTest = mapM_ test' [1..11]

-- Дальше результат переопредения всего и вся для того, чтобы проверить решение задачи
-- через моноид на аппликативных функторах. Полное решение не заморачиваясь
-- можно увидеть функцией performMonoidTest.

-- Здесь maxApp заменил на mconcat.
testPositive' i iter cons decons = do
    (rndS, rndN) <- makeRandom iter cons
    let
        rndMax = maximum rndN
        rndSomeMax = mconcat rndS
    testResult i $ rndMax == decons rndSomeMax

-- И здесь также.
testNegative' i iter cons neutral = do
    (rnd1, _) <- makeRandom iter cons
    (rnd2, _) <- makeRandom iter cons
    let
        args = rnd1++[neutral]++rnd2
        someMax = mconcat args
    testResult i $ someMax == neutral

-- А здесь просто везде добавил штришки.
test' :: Int -> IO ()
test' 1 = testPositive' 1 100 Just fromJust
test' 2 = testNegative' 2 25 Just Nothing
test' 3 = testPositive' 3 100 Right (\(Right r) -> r)
test' 4 = testNegative' 4 25 Right (Left "abc")
test' 5 = testPositive' 5 10 (:[]) (\ (x:xs) -> x)
test' 6 = testNegative' 6 25 (:[]) []
test' 7 = testPositive' 7 15 (replicate 2) (\ xs -> minimum xs)
test' 8 = testNegative' 8 20 (replicate 2) []
test' 9 = testPositive' 9 100 (ZipList . (replicate 2)) (minimum . getZipList)
test' 10 = testNegative' 10 25 (ZipList . (replicate 2)) (ZipList [])
test' 11 = do
    (rndIO, rndN) <- makeRandom 100 return
    let
        rndMax = maximum rndN
    rndIOMax <- mconcat rndIO
    testResult 11 $ rndMax == rndIOMax
