{-# LANGUAGE RankNTypes, NoMonomorphismRestriction #-}

import AbstractQueue
import qualified Queue as Q
import qualified FastQueue as FQ
import System.Environment
import System.Random
import qualified Data.Sequence as S





-- Основные операции с очередями    

type QueueFunction a = (AbstractQueue q) => (q a -> q a)

enqueueSome :: [a] -> QueueFunction a
enqueueSome els q = foldl enqueue q els

dequeueSome :: Int -> QueueFunction a
dequeueSome c q 
    | c==0 || isEmpty q = q
    | otherwise = dequeueSome (c-1) $ snd $ dequeue q

-- Проверка на равенство очередей

areEqual :: (AbstractQueue q1, AbstractQueue q2, Eq t) => q1 t -> q2 t -> Bool
areEqual q1 q2
    | isEmpty q1 && isEmpty q2 = True
    | isEmpty q1 = False
    | isEmpty q2 = False
    | otherwise = e1 == e2 && (areEqual qs1 qs2)
    where
        (e1, qs1) = dequeue q1
        (e2, qs2) = dequeue q2

{-
    Из-за определения внутри класса Eq (не)равенства, как a -> a -> Bool
    функцией (==) возможно сравнивать только очереди одинакового типа.
    Что делает ее неудобной при сравнений очередей построенных разными конструкторами,
    но принадлежащих типам, которые реализуют экземпляры класса AbstractQueue.
    
    instance (AbstractQueue q, Eq t) => Eq (q t) where
        q1 == q2 = areEqual q1 q2
        
    И это достаточно интересно, потому что следующий вариант не сработал бы
    используй я (==) :)
-}

equalityTest = areEqual que1 (snd $ dequeue que2)
    where
        que1 = enqueue (enqueue empty 1) 4 :: Q.Queue Int
        que2 = enqueue (enqueue (enqueue empty 7) 1) 4 :: FQ.Queue Int
        





{-
    Некая обстрактная общая задача.
    Класс типов AbstractTuple соответствует абстрактному кортежу из неопределенных очередей. 
    Поскольку все действия с очередями однотипны, а вторая задача может рассматриваться как
    обобщение первой, решил сделить подобную вещь, чтобы писать меньше "однотипностей".
-}

class AbstractTuple tup where
    mapFuncToTuple :: QueueFunction a -> tup a -> tup a -- что-то "около" функтора
    containsEqual :: (Eq a) => tup a -> Bool -- проверяет на равенство все элементы
    emptyTuple :: tup a -- дает кортеж пустых очередей.

type QueueingStep a = ([a], Int)

-- Один шаг работы с очередями
performQueueing :: (AbstractTuple t) => t a -> QueueingStep a -> t a
performQueueing tuple (els, c) = mapFuncToTuple (dequeueSome c) enqueued
    where
        enqueued = mapFuncToTuple (enqueueSome els) tuple

-- Получение списка случайных элементов.

makeRandom :: (Random a) => Int -> (a,a) -> IO [a]
makeRandom count bnds = newStdGen >>= return . (take count) . randomRs bnds

-- Делит множество элементов на подсписки и ставит в соответствие число
-- (длина подсписка минус 1)
divideElems :: [a] -> [QueueingStep a]
divideElems = makeAnswer . foldl divFunc ([], 1, 0, [])
    where
        divFunc (acc,mc,c,es) e
            | c == mc - 1   = ((e:es):acc, mc+1, 0, [])
            | otherwise = (acc, mc, c+1, e:es)
        makeAnswer (xs, _, _, _) = reverse $ zip xs $ map ((subtract 1). length) xs

makeRandomSteps :: (Random a) => Int -> (a,a) -> IO [QueueingStep a]
makeRandomSteps c bnds = makeRandom count bnds >>= return . divideElems
    where
        count = (c * (c+ 1)) `div` 2

-- Обобщенный тест для абстрактных кортежей из очередей.
testGeneralized :: (AbstractTuple t, Random a, Eq a) => t a -> Int -> (a, a) -> IO Bool
testGeneralized emptyT n bnds = do
    steps <- makeRandomSteps n bnds
    let 
        finishTuple = foldl performQueueing emptyT steps
    return $ containsEqual finishTuple

{- Конец обобщенной части -}



-- Агрументы:
-- 1) номер задания
-- 2) количество итераций вставок/удалений
-- 3,4) нижняя и верхняя границы случайной величины соответственно
main = do
    str <- getArgs
    let 
        (numStr:countStr:argsStr) = str
        n = read numStr :: Int
        c = read countStr :: Int
        (l:h:_) = map read argsStr :: [Int]
    answer <- test n c (l,h)
    if answer
        then putStrLn "Тест пройден! :)"
        else putStrLn "Что-то пошло не так :("
        
test :: (Random a, Eq a) => Int -> Int -> (a,a) -> IO Bool
test 1 = testGeneralized (emptyTuple :: DefTuple a) -- первое задание
test 2 = testGeneralized (emptyTuple :: Trio a) -- второе задание 
test _ = error "Такой задачи не было!!"





-- Задание 1     

data DefTuple a = DefTuple (Q.Queue a) (FQ.Queue a)

instance AbstractTuple DefTuple where
    containsEqual (DefTuple q1 q2) = areEqual q1 q2
    mapFuncToTuple f (DefTuple a b) = DefTuple (f a) (f b)
    emptyTuple = DefTuple (Q.empty) (FQ.empty)
    


-- Задание 2
-- Решил писать новый тип в том же файле, чтобы не загромождать вас файлами.
-- Хотя это вроде как противоречит идее инкапсуляции.

newtype SeqQueue t = SeqQueue (S.Seq t)

instance AbstractQueue SeqQueue where
    empty = SeqQueue (S.empty)
    isEmpty (SeqQueue s) = S.null s
    enqueue (SeqQueue s) e = SeqQueue ((S.|>) s e)
    dequeue (SeqQueue s) = (leftEnd, SeqQueue other)
        where
            (S.:<) leftEnd other = S.viewl s

data Trio a = Trio (Q.Queue a) (FQ.Queue a) (SeqQueue a)

instance AbstractTuple Trio where
    containsEqual (Trio q1 q2 q3) = areEqual q1 q3 && areEqual q2 q3
    mapFuncToTuple f (Trio a b c) = Trio (f a) (f b) (f c)
    emptyTuple = Trio (Q.empty) (FQ.empty) (empty::SeqQueue a)
