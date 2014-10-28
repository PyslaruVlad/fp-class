{-
  Соберите следующую информацию по текстовому файлу kapitan.txt:

  1) самый часто используемый знак препинания;
  2) 50 наиболее часто используемых в тексте слов (с указанием количества использований);
  3) частоты символов, биграмм и триграмм (вывести соответствующую информацию для
     наиболее часто встречающихся);
  4) количества использованных предлогов, местоимений и имён собственных.

  Постарайтесь использовать в решении наиболее подходящие структуры данных, избегайте повторные
  вычисления и заведомо неэффективные операции. Проверьте написанную программу на трёх других
  текстах того же или большего размера. Сравните результаты.
-}

{-# LANGUAGE FlexibleInstances #-}

import Data.List
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map.Lazy as LMap
import System.Environment



-- Красивый вывод ответа.
-- Вряд ли это стоило того, но я решил так сделать, чтобы нормально печатались русские
-- буквы/слова.

class Printable a where
    printPble :: Printable a => a -> IO ()

instance Printable Char where
    printPble c = putStr [c]
    
instance Printable [Char] where
    printPble s = putStr s

printRusFreqs :: Printable  a => [(a, Int)] -> IO ()
printRusFreqs [] = return ()
printRusFreqs ((e, i):es) = do
    printPble e >> putStr (" "++(show i)++"\n")
    printRusFreqs es




-- Чистая обобщенная задача. Составляет частотный словарь.

getUniqueFrequency :: Ord a => [a] -> [(a, Int)]
getUniqueFrequency elems = makeList $ makeMap (+) $ map makePair $ sort elems
    where
        makePair a = (a, 1)
        makeMap = LMap.fromAscListWith
        makeList = (sortBy comparePairs) . LMap.toList
    
comparePairs :: (a, Int) -> (a, Int) -> Ordering
comparePairs (_, n) (_, m)
    | n > m     = LT
    | n < m     = GT
    | otherwise = EQ




-- Некоторые чистые функции.

puncts :: Set.Set Char
puncts = Set.fromList ". ,{[}]-()<>.'\":?`;!"

getFirstPunct :: String -> Maybe (Char, Int)
getFirstPunct = getAns . getUniqueFrequency . punctInText
    where
        getAns [] = Nothing
        getAns xs = Just (head xs)
        punctInText = filter (\ s -> (Set.member s puncts) && (s /= '\n'))

getWords :: String -> [String]
getWords = filter (/="-") . (concatMap words) . lines . (filter pred)
    where
        otherPuncts = Set.difference puncts (Set.fromList " -") 
        pred = not . flip Set.member otherPuncts

ngrams :: Int -> String -> [String]
ngrams n str = iterFunc (length str) str
    where
        iterFunc m s
            | m < n     = []
            | otherwise = (take n s) : (iterFunc (m-n) $ drop 1 s)




-- Метод main и функции-задачи

main = do
    [taskNumStr , fname] <- getArgs
    contents <- readFile fname
    task (read taskNumStr) contents
    
    

task :: Int -> String -> IO ()

task 1 str = do
    let fp = getFirstPunct str
    if fp == Nothing
        then putStr "В данном файле не встречается символов пунктуации."
        else (do
            let (fs, ff) = fromJust fp
            putStr "Символ пунктуации, имеющий наибольшую частоту встречемости: \'"
            printPble fs >> putStr ("\'\nВстречается "++(show ff)++" раз.\n"))

task 2 str = do
    let 
        wrds = take 50 $ getUniqueFrequency $ getWords str
    putStrLn "50 (или меньше) наиболее часто встречаюшихся слов:"
    printRusFreqs wrds
        

task 3 str = do
    let
        symbols = filter (\s -> s /= '\n') str
        bigrams = ngrams 2 symbols
        trigrams = ngrams 3 symbols
    putStrLn "Частоты символов:"
    printRusFreqs $ getUniqueFrequency symbols
    putStrLn "Частоты биграмм:"
    printRusFreqs $ getUniqueFrequency bigrams
    putStrLn "Частоты триграмм:"
    printRusFreqs $ getUniqueFrequency trigrams

task 4 str = error "не сделал! :("


task _ _ = error "Такой задачи не было!"





