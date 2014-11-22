import Control.Monad
import Control.Monad.Reader
import System.Environment
import Data.Char

main = do
    [confName,numName] <- getArgs
    confStr <- readFile confName
    numStr <- readFile numName
    let
        confApplicator = getNumReader confStr
        nums = map read $ lines numStr
        answers = runReader confApplicator `liftM` nums
    print answers

getFuncByName :: String -> Int -> Int -> Int
getFuncByName "multiplier" f = \ x -> x * f
getFuncByName "summand" f = \ x -> x + f
getFuncByName "divisor" f = \ x -> x `div` f

-- Эта функция не пропускает пустые строки, поэтому я добавил много лишних enter'ов
-- в "01-config.txt" красоты ради. Также функция не пропускает ошибочные строки.
parseLine :: String -> Maybe (String, Int)
parseLine = verify . span (/= '=')
    where
        verify (name, field)
            | not $ name `elem` ["multiplier", "summand", "divisor"] = Nothing
            | field == ""  ||  head field /= '='  ||  isNotNum (tail field) = Nothing
            | otherwise = Just (name, read $ tail field)
        isNotNum fs = not $ foldr (\ c acc -> (isNumber c || c=='-') && acc) True fs

getNumReader :: String -> Reader Int Int
getNumReader = foldl parse ask . lines
    where
        parse prevR curL = combine prevR $ parseLine curL
        combine prevR Nothing = prevR -- Игнорирование ошибочных строк.
        combine prevR (Just (n, f)) = prevR >>= return . getFuncByName n f
        

{-
    Результат теста
         
        :main 01-config.txt 01-nums.txt
    
        [34,35,36,46,78,29,33,26]
    
    
    
    Если в конец "01-config.txt" дописать "summand=-33"
    то получим ровно ту же цепочку чисел, что и были на входе:
    
        [1,2,3,13,45,-4,0,-7]
-}
