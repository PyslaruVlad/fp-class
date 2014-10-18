{-
  Написать программу, которая в зависимости от параметров командной строки
  а) генерирует случайный текстовый файл содержащий декартовы координаты точек на плоскости
     (по одной точке в каждой строке);
  б) определяет по заданному файлу в указанном ранее формате количество точек в каждой
     из четвертей;
  в) отыскивает наиболее удалённую от начала координат точку.
-}

import System.Environment
import System.Random


task :: (Eq a, Fractional a) => a -> [String] -> IO ()
task 1 = startRandomize
task 1.5 = startRandomizeWithBounds
task 2 = findAlloc
task 3 = findDistant
task _   = error "такой задачи нет!"

main = do
    (num:args) <- getArgs
    task (read num) args



-- Задание 1
type Bounds = (Double, Double, Double, Double) 

randomPoint :: Bounds -> IO String
randomPoint (xMin, xMax, yMin, yMax) = do
    g <- newStdGen
    let
        (x, newGen) = randomR (xMin,xMax) g :: (Double, StdGen)
        y = fst $ randomR (yMin,yMax) newGen :: Double
    return $ unwords [show x,show y]

            
randomFile :: (String, Int, Bounds) -> IO ()
randomFile (fname, nLns, bnds) = do
    if nLns == 0
        then return ()
        else (do
            newStr <- randomPoint bnds
            appendFile fname $ newStr++"\n"
            randomFile (fname, nLns-1, bnds))
    
-- Разница между этой и следующей за ней функциями в том, что первая позволяет
-- пользователю самому выбрать границы области из которой случайно берется точка.    
startRandomizeWithBounds args@[fname, nLns, xMin, xMax, yMin, yMax] = do
    let
        b = map read $ drop 2 args :: [Double]
        bnds@(xMin, xMax, yMin, yMax) = (b!!0, b!!1, b!!2, b!!3)
    writeFile fname ""
    randomFile (fname, read nLns, bnds)
    
    
startRandomize [fname, nLns] = 
    startRandomizeWithBounds [fname, nLns, "-20", "20", "-20", "20"]




-- Задание 2
type Allocation = (Int, Int, Int, Int)

proccessPnt :: Allocation -> String -> Allocation
proccessPnt (p1, p2, p3, p4) str
    | x > 0 && y > 0 = (p1+1, p2, p3, p4)
    | x < 0 && y > 0 = (p1, p2+1, p3, p4)
    | x < 0 && y < 0 = (p1, p2, p3+1, p4)
    | x > 0 && y < 0 = (p1, p2, p3, p4+1)
    | otherwise      = (p1, p2, p3, p4)
    where
        (x:y:xs) = map read $ words str :: [Double]
        
findAlloc [fname] = do
    contents <- readFile fname
    let
        (p1,p2,p3,p4) = foldl proccessPnt (0,0,0,0) $ lines contents
    putStr $ "В первой четверти: "++(show p1)++"\n"
    putStr $ "Во второй четверти: "++(show p2)++"\n"
    putStr $ "В третьей четверти: "++(show p3)++"\n"
    putStr $ "В четвертой четверти: "++(show p4)++"\n"
    


-- Задание 3
compareFunc :: (Double, Double, Double) -> String -> (Double, Double, Double)
compareFunc old@(px, py, pr) str
    | nr > pr   = (x, y, nr)
    | otherwise = old
    where
        (x:y:xs) = map read $ words str :: [Double]
        nr = sqrt (x*x + y*y) :: Double


findDistant [fname] = do
    contents <- readFile fname
    let
        (x, y, _) = foldl compareFunc (0,0,0) $ lines contents
    putStr $ "Наиболее далекая точка имеет координаты: "++(show x)++", "++(show y)++"\n"
