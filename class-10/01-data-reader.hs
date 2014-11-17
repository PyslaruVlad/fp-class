{-
   Дан текстовый файл, содержащий данные о нескольких студентах в следующем формате: три последовательные
   строки соответствуют имени, возрасту и номеру группы (например, 4.8). Определить соответствующий тип
   данных (data) и организовать чтение файла в список значений этого типа.

   Для двух данных файлов объединить загруженные списки в один список, упорядоченный по имени и сохранить
   результат в новый файл того же формата. Указание: всюду следует использовать монадический синтаксис
   (операции >>= и >>, функции ap, liftM и другие функции монадической обработки данных, использование
   блока do не допускается).
-}

import Control.Monad
import System.Environment
import Data.Monoid

data Student = Student
    { name :: String
    , age :: Int
    , group :: String
    }

instance Show Student where
    show (Student n a g) = n ++ "\n" ++ (show a) ++ "\n" ++ g ++ "\n"
    
-- Считаем что множество строк имеет заведомо правильный размер (кратный 3).
readStudents :: [String] -> [Student]
readStudents [] = []
readStudents (n:a:g:ss) = Student n (read a) g : readStudents ss

-- Допускаем, что файл правильно форматирован.
readStudentsFromFile :: String -> IO [Student]
readStudentsFromFile fname = readFile fname >>= return . readStudents . lines

main = getArgs >>= mapM readStudentsFromFile 
        >>= (writeFile "01-studs-all.txt") . concatMap show . mconcat

{-
    Из-за опасений загрузить Вас большим количеством файлов, привожу их содержимое
    и результат здесь:
    
    Содержимое "01-studs-1.txt":
        Till
        18
        12.5
        Lindemann
        13
        0
    
    Содержимое "01-studs-2.txt":
        John
        23
        11
        Bon
        21
        4.9
        Jovy
        15
        1983
    

    После вызова :main 01-studs-1.txt 01-studs-2.txt 
    содержимое файла "01-studs-all.txt":
        Till
        18
        12.5
        Lindemann
        13
        0
        John
        23
        11
        Bon
        21
        4.9
        Jovy
        15
        1983
-}
