{-
   Дописать к реализованному ранее алгоритму Грэхема основную программу, которая принимает
   на вход файл со списком точек и создаёт файл со списком точек, образующих выпуклую оболочку.

   Для отыскания пути к импортируемому модулю следует использовать параметр -i командной строки
   (для ghc и ghci), например:

     $ ghc 05-graham.hs  -o graham -i../class-05/3-Graham_scan/
-}

import GrahamScan
import System.Environment

readPoints :: String -> [Point]
readPoints str = foldr readLine [] $ lines str
    where
        readLine ln acc =
            let (x:y:_) = map read $ words ln :: [Double]
            in (Point x y : acc)

writePoints :: [Point] -> String
writePoints =  foldr addP ""
    where
        addP (Point x y) acc = (show x)++" "++(show y)++"\n"++acc

main = do
    [inpName, outName] <- getArgs
    content <- readFile inpName
    let 
        pnts = graham_scan $ readPoints content
    writeFile outName $ writePoints pnts
    putStr "Хотите вывести овтет в консоль?\n(напишите y или"
    putStr " просто нажмите enter в противном случае)\n"
    input <- getLine
    if (input == "y")
        then putStr $ show pnts ++ "\n"
        else return ()
