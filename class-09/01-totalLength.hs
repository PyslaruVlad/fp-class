import System.Environment
import Data.Functor

{-
  Написать функцию, которая по заданному списку строк возвращает сумму длин всех строк.
-}

totalLength :: [String] -> Int
totalLength = sum . map length 

{-
  Написать функцию, которая по заданному символу и целому числу n строит список строк,
  содержащих 1, 2, ..., n повторений символа. Функция должна возвращать Nothing, если n=0.
-}

build1 :: Char -> Int -> Maybe [String]
build1 _ 0 = Nothing
build1 c n =  fmap (fmap $ flip replicate c) $ Just [1..n]

{-
  Написать функцию, аналогичную по возможностям функции build1, но возвращающую при этом
  значение Either String [String], в котором значение слева должно свидетельствовать об
  одной из следующих особых ситуаций: 
  (*) n=0;
  (*) n > 100;
  (*) Роспотребнадзор запрещает создавать строки из символа 'x'.
-}

build2 :: Char -> Int -> Either String [String]
build2 _ 0 = Left "n = 0"
build2 'V' _ = Left "Norsefire issued the order that sybmol 'V' is forbidden."
build2 c n
    | n> 100    = Left "n > 100"
    | otherwise = Right $ map (flip replicate c) [1..n]


{-
  Параметрами командной строки являются имя файла, символ, целое число.
  1) Пользуясь функцией totalLength и возможностями IO, как функтора, подсчитать и
     вывести общую длину строк, переданных программе в качестве аргументов командной строки.
  2) Пользуясь функцией totalLength и возможностями IO, как функтора, подсчитать и вывести общую
     длину строк, содержащихся в заданном текстовом файле (результат readFile должен быть
     предварительно преобразован к списку строк).
  3) Пользуясь функцией totalLength, подсчитать общую длину строк для значений в контекстах,
     сформированных функциями build1 и build2 (в решении следует пользоваться возможностями
     Maybe и Either String как функторов).
-}

task :: Int -> IO ()
task 1 = putStr "Количество символов, введенных вами: " 
    >> fmap (show . totalLength) getArgs >>= putStrLn 
task 2 = putStr "Количество символов, в файле: "
    >> getArgs >>= fmap (show . totalLength . lines) . readFile . head
    >>= putStrLn
task 3 = do
    (_:cStr:nStr:_) <- getArgs
    let
        (c:_) = cStr
        n = read nStr
        ansFunc f = fmap totalLength $ f c n 
        ans1 = ansFunc build1
        ans2 = ansFunc build2
    putStr "Результаты третьего задания: "
    putStr $ show ans1
    putStr " и "
    putStrLn $ show ans2


main = do
    task 1
    task 2
    task 3
