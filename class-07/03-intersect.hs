{-
  В параметрах командной строки указаны имена текстовых файлов, содержащих целые числа, разделённые
  пробелами и символами перевода строк. Определить количество и вывести различные числа, встречающиеся
  в каждом из заданных текстовых файлов. Указание: в решении следует воспользоваться множествами.
-}

import System.Environment
import qualified Data.Set as Set

-- добавил сюда Read. Надеюсь оправданно.
readNumFile :: (Num a, Read a) => FilePath -> IO [a]
readNumFile name = do 
    contents <- readFile name
    let
        ans = map read $ concatMap words $ lines contents
    return ans



solve :: (Num a, Ord a) => [[a]] -> (Int, [a])
solve = formAns . countInts . (map Set.fromList)
    where
        formAns xs = (length xs, xs)
        fF = Set.intersection
        countInts (s:ss) = Set.toList $ foldr fF s ss

main = getArgs >>= mapM readNumFile >>= print.solve
