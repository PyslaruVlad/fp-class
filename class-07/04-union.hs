{-
  В параметрах командной строки указаны имена текстовых файлов, содержащих целые числа, разделённые
  пробелами и символами перевода строк. Определить количество и сумму различных чисел, встречающихся
  в заданных текстовых файлах.
-}

import System.Environment
import qualified Data.Set as S

main = do
    args <- getArgs
    sets <- mapM readNumFile args
    let 
        ans = S.toList $ S.unions sets
    print (length ans, sum ans)
    
readNumFile :: (Num a, Ord a, Read a) => FilePath -> IO (S.Set a)
readNumFile fname = do
    contents <- readFile fname
    let
        output = map read $ concatMap words $ lines contents
    return $ S.fromList output
