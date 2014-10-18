{-
  Дан текстовый файл с информацией о студентах факультета в следующем формате:

    ФАМИЛИЯ ИМЯ ОТЧЕСТВО;ВОЗРАСТ;КУРС;ГРУППА

  Имя этого файла задаётся параметром командной строки. Остальные параметры определяют,
  какое действие следует выполнить:

  1) Вычислить средний возраст студентов заданной группы заданного курса.
  2) Вычислить количество студентов в каждой группе каждого курса.
  3) Создать файлы (с именами "<КУРС>_<ГРУППА>.txt") со списками всех студентов групп в формате
        ФАМИЛИЯ И.О.

-}

import System.Environment
import Data.Char
import qualified Data.List as Lst

task :: Int -> [String] -> IO ()
task 1 = calcAverAge
task 2 = calcAllocation
task 3 = allocateInGroups
task _ = error "такой задачи нет!"

main = do
    (num:args) <- getArgs
    task (read num) args


data Student = Student
    {
        surename :: String,
        firstname :: String,
        secondname :: String,
        age :: Int,
        year :: Int,
        group :: Int
    } deriving (Eq, Ord)

instance Show Student where
    show std = chainShow $ unwords [srnm,fstnm,sndnm] :
                        map show [age std, year std ,group std]
        where
            chainShow = foldr1 (\ s acc -> s++";"++acc)
            srnm = surename std
            fstnm = firstname std
            sndnm = secondname std


-- Тут я пока не смог написать успешную реализацию экземляра класса Read для типа Student
readStudent :: String -> Student
readStudent str = Student srn fstn sndn a y g
    where
        divStr = fst $ foldr divF ([], []) (';':str)
        divF c (acc, part)
            | c==';'    = (part:acc, [])
            | otherwise = (acc, c:part)
        (srn:fstn:sndn:_) = words $ map toUpper $ head divStr
        (a:y:g:_) = map read $ tail divStr :: [Int]
            



-- Задание 1

calcAverAgeStr :: String -> Double
calcAverAgeStr str = result $  foldl func (0, 0) $ lines str
    where
        func :: (Int, Int) -> String -> (Int, Int)   
        func (c, s) stdStr = (c+1, (s +) $ age $ readStudent stdStr)
        result (c, s)
            | c == 0    = 0
            | otherwise = fromIntegral s / fromIntegral c

calcAverAge [fname] = do
    contents <- readFile fname
    print $ calcAverAgeStr contents






-- Задание 2

type Group = (Int, Int, [Student])

calcAlloc :: String -> [Group]
calcAlloc sstr = foldr perfAllocStd [] $ lines sstr
    where
        perfAllocStd stdStr alloc = performInsert std alloc
            where
                std = readStudent stdStr
                performInsert is [] = [(year is, group is, [is])]
                performInsert is (cU@(cy, cg, css):als)
                    | (year is == cy) && (group is) == cg = (cy, cg, (is):css):als
                    | otherwise = cU : performInsert is als


printAllocation :: [Group] -> String
printAllocation alloc = foldr printUnit [] $ Lst.sort alloc
    where
        printUnit (y, g, stds) acc = "На курсе "++(show y)++" в "++(show g)
                ++" группе "++(show $ length stds)++" человек.\n"++acc

calcAllocation [fname] = do
    contents <- readFile fname
    let
        alloc = calcAlloc contents
        output = printAllocation alloc
    putStr output
    
  




    
-- Задание 3

appendToGroup :: (String, [Student]) -> IO ()
appendToGroup (fname, (s:stds)) = do
    appendFile fname $ (show s)++"\n"
    if length stds == 0
        then return ()
        else appendToGroup (fname, stds)


addGroupFile :: [Group] -> IO ()
addGroupFile ((gy, gn, gstds):gs) = do
    let
        name = (show gy)++"_"++(show gn)++".txt"
    writeFile name ""
    appendToGroup (name, Lst.sort gstds)
    if length gs == 0
        then return ()
        else (addGroupFile gs)

-- Использую функцию из предыдущего задания
allocateInGroups [fname] = do
    contents <- readFile fname
    let
        alloc = calcAlloc contents
    addGroupFile alloc

