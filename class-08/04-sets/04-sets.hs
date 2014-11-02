{-# LANGUAGE RankNTypes, NoMonomorphismRestriction #-}

import AbstractSet
import qualified ListSet as L
import qualified TreeSet as T
import qualified Data.Set as S
import System.Environment
import System.Random
import Control.Monad




-- Для проверки
instance AbstractSet S.Set where
    empty = S.empty
    isEmpty = S.null
    member = S.member
    insert = S.insert
    delete = S.delete
    toAscList = S.toAscList





-- Приемущественно использую наработки из третьего задания.

-- Функции по обработке множеств.
type SetFunction a = (AbstractSet s) => (s a -> s a)

insertSome :: Ord a => [a] -> SetFunction a
insertSome ls set = foldr insert set ls

deleteSome :: Ord a => [a] -> SetFunction a
deleteSome ls set =  foldr delete set ls

containsSome :: (Ord a, AbstractSet s) => [a] -> s a -> Bool
containsSome ls set = foldr (\ e acc -> e `member` set && acc) True ls


-- Кортежи из разнотипных множеств и функции по их обработке
type SetTuple a = (S.Set a, L.ListSet a, T.TreeSet a)

mapFuncToTuple :: SetFunction a -> SetTuple a -> SetTuple a
mapFuncToTuple f (a, b, c) = (f a, f b, f c)

areEqual :: (AbstractSet s1, AbstractSet s2, Ord a) => s1 a -> s2 a -> Bool
areEqual set1 set2 = (toAscList set1) == (toAscList set2)

tupleIsGood :: Ord a => SetTuple a -> Bool
tupleIsGood (s1, s2, s3) = (areEqual s1 s2) && (areEqual s1 s3) && (areEqual s2 s3)

emptySetTuple :: SetTuple a
emptySetTuple = (empty, empty, empty)







-- Создание случайных элементов.
makeRandom :: Random a => Int -> (a,a) -> IO [a]
makeRandom count bnds = newStdGen >>= return . (take count) . randomRs bnds

-- Генерация случаных наборов случайных элементов
makeRandomGroups :: Random a => Int -> (a, a) -> IO [[a]]
makeRandomGroups count bnds = do
    if count == 0
        then return []
        else (do
            [argCount] <- makeRandom 1 (1,10)
            args <- makeRandom argCount bnds
            end <- makeRandomGroups (count-1) bnds
            return $ args:end
            )






-- Функция по случайному номеру.
absFunction :: Ord a => Int -> [a] -> SetFunction a
absFunction 1 = insertSome
absFunction 2 = deleteSome
absFunction _ = error "Error in function detection!"

-- Два разных типа действий над кортежем.
-- Действией функцией на кортеж.
performAbsFunction :: (Random a, Ord a) => Int -> SetTuple a -> [a] -> SetTuple a
performAbsFunction funcNum tuple ls = mapFuncToTuple (absFunction funcNum ls) tuple

-- Проверка на принадлежность некоторых элементов.
caseThreeStep :: (Random a, Ord a) => SetTuple a -> [a] -> Bool
caseThreeStep (s1, s2, s3) ls = (f == s) && (f == t)
    where
        f = containsSome ls s1
        s = containsSome ls s2
        t = containsSome ls s3






-- Шаг проверки.
performStep :: (Random a, Ord a) => SetTuple a -> [[a]] -> IO Bool
performStep tuple [] = return $ tupleIsGood tuple
performStep tuple (ls:ss) = do
    [funcNum] <- makeRandom 1 (1,3)
    if funcNum == 3
        then (do
            let current = caseThreeStep tuple ls
            next <- performStep tuple ss
            return $ current && next
            )
        else (do
            let newTuple = performAbsFunction funcNum tuple ls
            performStep newTuple ss
            )

-- Обобщенный тест.
testGeneralized :: (Random a, Ord a) => Int -> (a, a) -> IO Bool
testGeneralized n bnds = do
    steps <- makeRandomGroups n bnds
    performStep emptySetTuple steps







-- Ответ здесь и далее:

testingLoop :: (Random a, Ord a) => Int -> Int -> (a, a) -> IO Int
testingLoop cl n bnds = do
    if cl == 0
        then return 0
        else (do
            end <- testingLoop (cl-1) n bnds
            current <- testGeneralized n bnds
            if (current)
                then return $ 1 + end
                else return $ end
            )
  
-- count - число разных проверок.
-- deep - "глубина" проверки в каждом отдельном случае.
-- left, right - границы случайной величины.
-- Например, :main 100 100 (1.10)
-- Надеюсь мне зачтется то, что здесь как минимум три разных теста.
main = do
    argsStr <- getArgs
    let [count, deep, left, right] = map read argsStr :: [Int]
    rightCount <- testingLoop count deep (left, right)
    putStrLn $ "Завершено тестов: "++(show count)
    putStrLn $ "Из них пройдено: "++(show rightCount)
