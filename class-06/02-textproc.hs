{-
  Разработайте утилиту со следующими возможностями:
  1) подсчёт количества строк в заданном текстовом файле;
  2) добавление заданной строки в начало (конец) заданного файла;
  3) преобразование всех буквенных символов заданного файла к верхнему
     регистру (результат выводится на консоль);
  4) построчное слияние двух заданных файлов (каждая строка первого файла
     соединяется с соответствующей строкой второго файла);
  5) генерация случайного текстового файла (случайность должна ограничиваться
     максимальным количеством строк в файле и символов в строке).

  Все входные данные программы должны передаваться с помощью параметров
  командной строки.
-}

import System.Environment
import System.Random
import Data.Char


{-
    Ответ решил оформить вот в таком вот виде. Если хотите проверить конкретное задание,
    достаточно набрать что-то типо  :main n args
    где n - номер задания, args - аргументы конкретного задания (их может быть несколько).
    
    Например первое задание: :main 1 "hello.txt"
    Или второе: :main 2 "hello.txt" "abra cadabra"
    
    
    Все функции-ответы имеют сигнатуру "[String] -> IO ()". Поэтому для функций-ответов
    я в дальнейшем сигнатуру не пишу.
-}
task :: Int -> [String] -> IO ()
task 1 = countStrFromFile
task 2 = appendStr
task 3 = fileToUpper
task 4 = zipFiles
task 5 = startRandomize
task _ = error "такой задачи нет!"

main = do
    (num:args) <- getArgs
    task (read num) args



-- Задание 1
countStr :: String -> Int
countStr = foldr f 0
    where
        f '\n' a = a + 1
        f _ a = a
     
countStrFromFile [fname] = do
    contents <- readFile fname
    print $ countStr contents
    


-- Задание 2
appendStr [fname, str] = do
    appendFile fname $ '\n':str
    

        
-- Задание 3
strToUp :: String -> String
strToUp str = if last newStr == '\n' then newStr else newStr ++ "\n"
    where
        newStr = map toUpper str

fileToUpper [fname] = do
    contents <- readFile fname
    putStr $ strToUp contents

    
    
-- Задание 4
zipStrs :: String -> String -> String
zipStrs str1 str2 = unlines $ zipWith (++) (lines str1) (lines str2)

zipFiles [f1name, f2name] = do
    cnt1 <- readFile f1name
    cnt2 <- readFile f2name
    putStr $ zipStrs cnt1 cnt2


    
-- Задание 5
randomStr :: Int -> IO String
randomStr n = do
    gen <- newStdGen
    let
        lngth = fst $ randomR (1, n) gen
    return $ take lngth $ randomRs (' ', '~') gen

randomFile :: (String, Int, Int) -> IO ()
randomFile (fname, nLns, nChrs) = do
    if nLns == 0
        then return ()
        else (do
            newStr <- randomStr nChrs
            appendFile fname $ newStr++"\n"
            randomFile (fname, nLns-1, nChrs))
    
startRandomize [fname, nLns, nChrs] = do
    writeFile fname ""
    gen <- newStdGen
    let
        lns = fst $ randomR (1, read nLns) gen
    randomFile (fname, lns, read nChrs)
