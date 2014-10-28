{-
  Дан текстовый файл (его имя задано в параметрах командной строки), содержащий целые числа
  в диапазоне от 1 до 1000, разделённые пробелами и символами перевода строки. Определить
  количество различных чисел в нём, пользуясь для этого возможностями различных структур
  данных. 
-}

import Data.List
import qualified Data.Sequence as Seq
import qualified Data.IntSet as Set
import Data.Array.IArray
import System.Environment
import Control.Monad

nub_set :: Set.IntSet -> Int
nub_set = Set.size

nub_list :: [Int] -> Int
nub_list = length . nub

nub_seq :: Eq a => Seq.Seq a -> Int
nub_seq = Seq.length . viewAndDel
    where
        viewAndDel :: Eq a => Seq.Seq a -> Seq.Seq a
        viewAndDel = deleteCopies . Seq.viewl
        deleteCopies :: Eq a => Seq.ViewL a -> Seq.Seq a
        deleteCopies Seq.EmptyL = Seq.empty
        deleteCopies  (n Seq.:< ns) = (Seq.<|) n $ viewAndDel (Seq.filter (/=n) ns)
        
nub_arr :: Array Int Int -> Int
nub_arr arr = fst $ foldr fF (0, Set.empty) [f..l]
    where
        (f,l) = bounds arr
        fF n (c, st)
            | arr ! n `Set.member` st   = (c, st)
            | otherwise                 = (c+1, Set.insert (arr!n) st)

main = do
  [fname] <- getArgs
  content <- readFile fname
  let xs = map read $ concatMap words $ lines content
  let (n:results) = [
        nub_set $ Set.fromList xs,
        nub_list xs,
        nub_seq $ Seq.fromList xs,
        nub_arr $ listArray (1,length xs) xs ]
  mapM_ print results
  when (any (/= n) results) $ putStrLn "Результаты не совпадают!"
