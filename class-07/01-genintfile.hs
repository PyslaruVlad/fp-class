{-
  Создать текстовый файл, содержащий случайные целые числа, разделённые пробелами
  и символами перевода строки. В командной строке должны задаваться следующие параметры:
  1) имя создаваемого файла;
  2) диапазон генерируемых случайных чисел: от и до;
  3) количество чисел в строке;
  4) количество строк в файле.
-}

import System.Environment
import System.Random
import Control.Monad

main = do
    args <- getArgs
    let
        filename = args!!0
        first = read $ args!!1
        last = read $ args!!2
        qNums = read $ args!!3
        qStrs = read $ args!!4
    answer <- appendStrs qStrs (qNums, first, last)
    writeFile filename $ unlines answer
    



appendStrs :: Int -> (Int, Int, Int) -> IO [String]
appendStrs 0 _ = return []
appendStrs qStrs strArgs = do
    cur <- generateStr strArgs
    next <- appendStrs (qStrs-1) strArgs
    return $ cur:next
        


generateStr :: (Int, Int, Int) -> IO String
generateStr (q, l, h) = do
    g <- newStdGen
    return $ specPrint $ take q $ randomRs (l, h) g
        where
            specPrint = init . foldr (\ n acc -> show n++" "++ acc) ""

