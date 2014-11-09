{-# LANGUAGE RankNTypes, NoMonomorphismRestriction #-}
import System.Environment
import Data.Monoid
import Data.Maybe

{-
  Некоторый датчик генерирует по пять сигналов в сутки, часть из которых
  не доходит до базовой станции. Полученные от датчика сведения представлены
  текстовым файлом, содержащим по одному целому числу в каждом строке. Если
  сигнал не был получен, вместо числа в файле записывается прочерк (символ '-').
-}

type SensorValue = Maybe Int
type SensorData = [SensorValue]

{- Напишите функцию, которая преобразует прочитанную из файла строку в список
   значений, полученных от датчика. -}

getData :: String -> SensorData
getData = map eval . lines
    where
        eval "-" = Nothing
        eval strNum = Just $ read strNum 

{- Напишите функцию, группирующую данные по суткам. -}

dataByDay :: SensorData -> [SensorData]
dataByDay [] = []
dataByDay dat = take 5 dat : (dataByDay $ drop 5 dat)

{-
  Посчитайте минимальное значение среди показаний датчика,
  полученных:
  а) первыми в течение суток;
  б) последними в течение суток.
  Если в некоторые сутки показания не были получены ни разу,
  такие сутки должны игнорироваться.

  Указание: в решении следует пользоваться возможностями моноидов First и Last,
  при этом должна быть написана одна функция, отвечающая на вопрос а) или б)
  в зависимости от значения логического параметра.
-}

minData1 :: Bool -> [SensorData] -> Int
minData1 True = fromJust . getFirst . minimum . map (mconcat . map First)
minData1 False = fromJust . getLast . minimum . map (mconcat . map Last)

    
{-
  Посчитайте минимальное значение среди данных,
  полученных:
  а) как суммы всех показаний датчика за каждые сутки;
  б) как произведения всех показаний датчика за каждые сутки.
  Если в некоторые сутки показания не были получены ни разу,
  такие сутки должны игнорироваться.

  Указание: в решении следует пользоваться возможностями моноидов Sum, Product
  и Maybe a, где a — моноид, при этом должна быть написана одна функция, отвечающая
  на вопрос а) или б) в зависимости от значения логического параметра.
-}

minData2 :: Bool -> [SensorData] -> Int
minData2 True =  getSum . minimum . 
    map (mconcat . map (Sum . fromJust) . filter isJust)
minData2 False =  getProduct . minimum . 
    map (mconcat . map (Product . fromJust) . filter isJust)

{- Попробуйте объединить две предыдущие функции в одну. -}

data SensorTask = NeedFirst | NeedLast | NeedSum | NeedProduct

minData :: SensorTask -> [SensorData] -> Int
minData NeedFirst = minData1 True
minData NeedLast = minData1 False
minData NeedSum = minData2 True
minData NeedProduct = minData2 False


{-
  Пользуясь моноидами All, Any и любыми другими, выясните следующую информацию:
  1) количество суток, за которые не было получено ни одного показания;
  2) количество суток, показания за которые получены полностью;
  3) количество суток, за которые было получено хотя бы одно показание;
  4) количество суток, сумма показаний за которые превосходит заданное число;
  5) количество суток, произведение показаний за которые превосходит заданное число;
  6) количество суток, первое показание за которые превосходит заданное число;
  7) количество суток, последнее показание за которые превосходит заданное число.

  Постарайтесь ответить на все вопросы, написав одну функцию.
-}

countWith :: Monoid m => (m -> Bool) -> ([a] -> [m]) -> [[a]] -> Int 
countWith predicate construct = length . filter (predicate . mconcat . construct)

data Parameters = One Int | Two Int Int
        
task :: Parameters -> [SensorData] -> Int
task (One 1) = countWith getAll (map (All . isNothing))
task (One 2) = countWith getAll (map (All . isJust))
task (One 3) = countWith getAny (map (Any . isJust))
task (Two 4 n) = countWith ((> n) . getSum) (map (Sum . fromJust) . filter isJust)
task (Two 5 n) = countWith ((> n) . getProduct) (map (Product . fromJust) . filter isJust)
task (Two 6 n) = countWith (getAns n . getFirst) (map First)
task (Two 7 n) = countWith (getAns n . getLast) (map Last)
    
getAns :: Ord a => a -> Maybe a -> Bool
getAns _ Nothing = False
getAns n (Just a) = a > n 





instance Show Parameters where
    show (One i) = show i
    show (Two i _) = show i

printAns :: [SensorData] -> (Parameters, String) -> IO ()
printAns d (p,m) = do
    putStr $ (show p)++") "++m++": "
    print $ task p d
    
makeParameters :: Int -> [Parameters]
makeParameters n = onePart ++ twoPart
    where
        onePart = map One [1..3]
        twoPart = map (flip Two n) [4..7]

msg :: Int -> String
msg 1 = "Ни одного показания"
msg 2 = "Все показания"
msg 3 = "Хотя бы одно показание"
msg 4 = "Сумма превосходит"
msg 5 = "Произведение превосходит"
msg 6 = "Первое превосходит"
msg 7 = "Последнее превосходит"

-- пример: 
-- :main 03-file1.txt 3
main = do
  fname <- head `fmap` getArgs
  sData <- getData `fmap` readFile fname
  let
    dData = dataByDay sData
  (_:paramStr:_) <- getArgs
  let 
    p = read paramStr
    params = makeParameters p
    msgs = map msg [1..7]
    args = zip params msgs
  mapM_ (printAns dData) args
