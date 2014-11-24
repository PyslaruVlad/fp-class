{-#Language RankNTypes #-}
import Control.Monad
import Control.Monad.ST
import Data.STRef
import Data.Array
import Data.Array.ST
import Data.Array.MArray
import Control.Applicative
import Data.List

type CompF e = e -> e -> Ordering
type SortingFunction s i e = forall s . CompF e -> (i, i) ->  STArray s i e -> ST s ()


-- Функции из лекции.

swapElems :: Ix i => STArray s i e -> i -> i -> ST s ()
swapElems arr i j = do
    vi <- readArray arr i
    vj <- readArray arr j
    writeArray arr i vj
    writeArray arr j vi

sortWith :: CompF e -> SortingFunction s Int e -> [e] -> [e]
sortWith comF func xs = elems $ runSTArray $ do
    let bounds = (1, length xs)
    arr <- newListArray bounds xs
    func comF bounds arr
    return arr


-- Тесты.

args :: Int -> [Int]
args 1 = [1,3,46,2,7,8,9,4,2,4,6]
args 2 = [10,9..1]
args 3 = [1,11,1,1,1,1,1,11,1]
args 4 = [456,2,7,8,32,436,0,2,-0,-15,67]

compareZip :: ZipList [Int] -> ZipList [Int] -> Bool
compareZip l1 l2 = and $ getZipList $ (==) <$> l1 <*> l2

checkSorting = bubble && insertion && quick
    where
        simple = map (sortBy customCompare) lists
        bubble = performSort bubbleSort == simple
        insertion = performSort insertionSort == simple
        quick = performSort quickSort == simple
        performSort :: SortingFunction s Int Int -> [[Int]]
        performSort func = map (sortWith compare func) lists
        lists = map args [1..4]
        customCompare a b
            | a > b = GT
            | a < b = LT
            | otherwise = EQ
        





-- Сортировка пузырьком.

swapIfGT :: Ix i => CompF e -> STArray s i e -> i -> i -> ST s ()
swapIfGT comF arr i j = do
    vi <- readArray arr i
    vj <- readArray arr j
    let
        (ni, nj) = if comF vi vj /= GT then (vj, vi) else (vi, vj)
    writeArray arr i ni
    writeArray arr j nj

bubbleSort :: Ix i => SortingFunction s i e
bubbleSort comF (lB, uB) arr = mapM_ pickUp $ range (lB, uB)
    where
        pickUp i = mapM_ (swapIfGT comF arr i) $ range (lB, i)





-- Сортировка вставками.

insertSomewhere ::(Ix i, Enum i) => CompF e -> (i, i) -> STArray s i e -> ST s ()
insertSomewhere comF (lB, uB) arr
    | lB >= uB = return ()
    | otherwise = do
        vi <- readArray arr uB
        vn <- readArray arr $ pred uB
        if (comF vn vi /= GT)
        then return ()
        else do
        writeArray arr uB vn
        writeArray arr (pred uB) vi
        insertSomewhere comF (lB, pred uB) arr

insertionSort :: (Ix i, Enum i) => SortingFunction s i e
insertionSort comF (lB, uB) arr = mapM_ procElem $ range (lB, uB)
    where
        procElem i = insertSomewhere comF (lB, i) arr 




-- Быстрая сортировка.

-- Распределяет все элементы меньше данного в начало массива, а все элементы больше
-- в конец. Возвращает положение (индекс) последнего элемента попавшего в 
-- "меньше-либо-равную" группу. На его место потом встанет контрольный элемент.
reorderWith :: (Ix i, Enum i) => CompF e -> (i, i) -> STArray s i e -> e -> ST s i
reorderWith comF (lB, uB) arr vComp = do
        vl <- readArray arr lB
        vu <- readArray arr uB
        let
            condL = comF vl vComp /= GT
            condU = comF vu vComp == GT
        if lB >= uB
        then return $ checkLast condU uB
        else do
        let
            newL = if condL then succ lB else lB
            newU = if condU then pred uB else uB
        if (newL, newU) /= (lB, uB)
        then 
            reorderWith comF (newL, newU) arr vComp
        else do
            writeArray arr lB vu
            writeArray arr uB vl
            reorderWith comF (succ lB, pred uB) arr vComp

-- Проверяет последний элемент функции reorder. В зависимости от результат
-- чуточку изменяет значение индекса.
checkLast :: (Ix i, Enum i) => Bool -> i -> i
checkLast cond i
    | cond = pred i
    | otherwise = i

quickSort :: (Ix i, Enum i) => SortingFunction s i e
quickSort comF (lB, uB) arr
    | lB >= uB = return ()
    | otherwise = do
        vL <- readArray arr lB
        midPos <- reorderWith comF (lB, uB) arr vL
        quickSort comF (succ midPos, uB) arr
        swapElems arr lB midPos
        quickSort comF (lB,pred midPos) arr
