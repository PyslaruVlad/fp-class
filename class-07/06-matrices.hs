{-
   Напишите программу обработки квадратных матриц (на массивах) со следующими возможностями:
   1) чтение матрицы из тестового файла;
   2) запись матрицы в текстовый файл;
   3) сумма матриц;
   4) произведение матриц.

  Задание на обработку может выглядеть, к примеру, следующим образом (здесь вычисляется матричное
  выражение (A + B) * C):

    LOAD a FROM matr1.txt
    LOAD b FROM matr2.txt
    LOAD c FROM matr3.txt
    CALC d AS PLUS a b
    CALC e AS MULT d c
    SAVE d TO matr4.txt

   Параметром командной строки должно быть имя файла со сценарием указанного или подобного ему вида.
-}

import System.Environment
import Data.Char
import Data.List
import qualified Data.Array as Arr
import qualified Data.Map.Lazy as Map





-- Немного чистоты.

type Matrix a = Arr.Array (Int, Int) a

createMatrixFromOneRow:: Int -> Int -> [a] -> Matrix a
createMatrixFromOneRow nRow nCol vals = makeArr nRow nCol vals
    where
        makeArr mR mC sV = Arr.listArray ((1,1),(mR,mC)) sV



readMatrix :: (Read a) => String -> Matrix a
readMatrix str = createMatrixFromOneRow nRow nCol elemList
    where
        (lns, nRow) = (lines str, length lns)
        (wds, nCol) = (map words lns, length $ head wds)
        elemList = map read $ concat wds :: Read a => [a]
       


writeMatrix :: (Show a) => (Matrix a) -> String
writeMatrix matrix = unlines $ showMatrix $ map show $ Arr.elems matrix
    where
        (_,( _, nCol)) = Arr.bounds matrix
        showMatrix [] = []
        showMatrix xs = (unwords row) : showMatrix other 
            where
                (row, other) = splitAt nCol xs







-- Алгебра матриц

proccessMatrices_lin :: (a -> b -> c) -> Matrix a -> Matrix b -> Matrix c
proccessMatrices_lin f a b = getAns
    where
        aBds@(_, (aR, aC)) = Arr.bounds a
        getAns = Arr.array aBds pcdM
        pcdM = [((i,j) , f (ij a (i,j)) (ij b (i,j))) | i <- [1..aR], j <- [1..aC]]
            where
                ij m (k, p) = (Arr.!) m (k,p)

proccessMatrices_nonLin :: (a -> b -> c) -> ([c] -> d) -> Matrix a -> Matrix b -> Matrix d
proccessMatrices_nonLin mF sF a b = getAns
    where
        aBds@(_, (aR, aC)) = Arr.bounds a
        bBds@(_, (bR, bC)) = Arr.bounds b
        getAns = Arr.array ((1,1), (aR, bC))pcdM
        pcdM = [((i,j) , sF $ sumAB i j) | i <- [1..aR], j <- [1..bC]]
            where
                eL m i j = ((Arr.!) m (i,j))
                sumAB i j = foldr (\ n acc -> mF (eL a i n) (eL b n j): acc) [] [1..aC]




-- Тесты. Не выдают ни True, ни False, но просто красиво печатают ответ.

matrixTest :: (Matrix String -> Matrix String -> Matrix String) -> IO ()
matrixTest proccessMatrs = 
    putStr $ writeMatrix $ proccessMatrs aMat bMat
    where
        makeMat = createMatrixFromOneRow 2 2
        aMat = makeMat aStr
        bMat = makeMat bStr
        aStr =  ["a11", "a12", "a21", "a22"]
        bStr =  ["b11", "b12", "b21", "b22"]
                

test_proccessMatrices :: Int -> IO ()
test_proccessMatrices 1 = matrixTest $ proccessMatrices_lin ((++) . (++"+"))
test_proccessMatrices 2 = matrixTest $ proccessMatrices_nonLin mF sF
    where
        mF = (++) . (++" * ")
        sF = concat . intersperse " + "
        
        




-- Команды.

type MatrixStore a = Map.Map String (Matrix a)
type CommandType a = [String] -> (MatrixStore a) -> IO (MatrixStore a)

loadFrom_com :: Read a => CommandType a
loadFrom_com [varName, fName] mStore = do
    contents <- readFile fName
    let
        rm = readMatrix contents
    return $ Map.insert varName rm mStore

calcAs_com :: Num a => CommandType a
calcAs_com (varName:funcName:opers) mStore = do
    let
        ms = foldr (\ str acc -> (Map.!) mStore str : acc) [] opers
        mo = calcFunc funcName ms
    return $ Map.insert varName mo mStore
    
calcFunc :: Num a => String -> [Matrix a] -> Matrix a
calcFunc "PLUS" = foldl1 (proccessMatrices_lin (+))
calcFunc "MULT" = foldl1 (proccessMatrices_nonLin (*) sum)


saveTo_com :: Show a => CommandType a
saveTo_com [varName, fName] mStore = do
    let
        mw = (Map.!) mStore varName
    writeFile fName $ writeMatrix mw
    return $ Map.delete varName mStore

{- 
    Парсинг строки аргументов для каждого из типов комманд.
    Не придумал ничего лучше, ибо "тип" команды определяется по первому слову а дальше
    может быть любая "белиберда" в перемешку с полезностями.
-}

type ParseType = [String] -> [String]

loadFrom_parse :: ParseType
loadFrom_parse (varName:from:fname:_) = [varName, fname]

calcAs_parse :: ParseType
calcAs_parse (varName:asS:funcName:operNames) = (varName:(map toUpper funcName):operNames)

saveTo_parse :: ParseType
saveTo_parse (varName:to:fname:_) = [varName, fname]

-- Отображение имен команд на функции команд.

type TupleOfCommands a = (CommandType a, ParseType)

comandsByName :: (Read a, Show a, Num a) => Map.Map String (TupleOfCommands a)
comandsByName = Map.fromList
    [("LOAD", (loadFrom_com, loadFrom_parse))
    ,("CALC", (calcAs_com, calcAs_parse))
    ,("SAVE", (saveTo_com, saveTo_parse))]





-- Интерпретация файла.

interpCommand :: (Read a, Show a, Num a) => CommandType a
interpCommand (cN:args) mStore = do
    let
        comName = map toUpper cN
        (command, parse) = (Map.!) comandsByName comName
        pArgs = parse args
    cur <- command pArgs mStore
    return cur


interpFileCycle :: (Read a, Show a, Num a) => MatrixStore a -> [String] -> IO (MatrixStore a)
interpFileCycle mStore (line:ls) = do
    newMStore <- interpCommand (words line) mStore
    if ls == []
        then return Map.empty
        else interpFileCycle newMStore ls


-- Окончательное решение.

main = do
    [scriptName] <- getArgs
    contents <- readFile scriptName
    let initStore = Map.empty :: MatrixStore Double
    interpFileCycle initStore $ lines contents
    return ()   
