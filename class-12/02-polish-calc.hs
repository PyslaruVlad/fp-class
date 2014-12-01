{-
   Представленный на лекции вычислитель выражений в обратной польской нотации
   не проверяет корректность заданного выражения, выдавая, к примеру, следующие
   очевидно неправильные ответы:

   ghci> evalRPN "* 1"
   1
   ghci> evalRPN "+ * 2 4"
   4
   ghci> evalRPN "* * *"
   *** Exception: 01-polish-calc.hs:10:15-43: Non-exhaustive patterns in lambda

   1. Переработайте реализацию вычислителя таким образом, чтобы в случае ошибки ответ
   не выводился. Воспользуйтесь в решении монадой Maybe, совместив её с монадой State
   с помощью преобразователя монад.

   2. Добавьте к вычислителю поддержку переменных. Значения переменных должны
   задаваться в командной строке, а доступ к ним должен осуществляться средствами
   монады Reader.

   3. Добавьте к вычислителю подсчёт количества операций со стеком (монада Writer
   с журналом типа Sum Int).
-}


import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Writer
import Control.Monad.Reader

import Data.Maybe
import Data.Char

type Stack = [Int]

push :: Int -> State Stack ()
push x = get >>= put . (x:)

pop :: State Stack Int
pop = get >>= \(x:xs) -> put xs >> return x

evalRPN :: String -> Int
evalRPN xs = head $ execState (mapM step $ words xs) []
  where
    step "+" = processTops (+)
    step "*" = processTops (*)
    step  n  = push (read n)
    processTops op = op `liftM` pop `ap` pop >>= push
    


    
-- Решение.

isOperation :: String -> Bool
isOperation e = e `elem` operations
    where operations = ["+","*"]

inCorrectCondition :: Stack -> String -> Bool
inCorrectCondition stack c = not (isOperation c && length stack < 2)
        
isCorrectSymbol :: (String -> Maybe Int) -> String -> Bool
isCorrectSymbol lF e = lF e /= Nothing || all isNumber e || isOperation e

performStep :: String -> ReaderT (String -> Maybe Int) (WriterT (Sum Int) (State Stack)) ()
performStep "+" = lift $ processTops (+)
performStep "*" = lift $ processTops (*)
performStep n  = do
    lookupFunc <- ask
    let 
        lookRes = lookupFunc n
        newVal = if isNothing lookRes then read n else fromJust lookRes
    lift $ lift $ push newVal

processTops :: (Int -> Int -> Int) -> WriterT (Sum Int) (State Stack) ()
processTops op = do
    tell (Sum 1) -- количество успешных операций со стеком.
    lift $ op `liftM` pop `ap` pop >>= push

processSymbol :: String -> MaybeT (ReaderT (String -> Maybe Int) 
    (WriterT (Sum Int) (State Stack))) ()
processSymbol c = do
    stack <- get
    lookupFunc <- ask
    if inCorrectCondition stack c && isCorrectSymbol lookupFunc c
    then lift $ performStep c
    else mzero

newEvalRPN :: (String -> Maybe Int) -> [String] -> (Maybe Int, Int)
newEvalRPN lookupFunc = checkMaybe . runAll . runMaybeT . mapM processSymbol
    where 
        checkMaybe ((Just _, c), stack) = (Just $ head stack, getSum c)
        checkMaybe ((Nothing, c), _) = (Nothing, getSum c)
        runAll = (flip runState []) . runWriterT . (flip runReaderT lookupFunc)






-- Тесты.

testRPN :: Bool
testRPN = positiveTest && negativeTest
    where
        positiveTest = test posTasks posAns
        negativeTest = test negTasks negAns
        test tasks ans = map (newEvalRPN dictFunc . words) tasks == ans
        posTasks = ["3 2 4 + *", "5 a *", "a b c 3 + + +", "c 5 + 3 *", "1 b 8 * +"]
        posAns = [(Just 18, 2), (Just 5, 1), (Just 15, 3), (Just 36, 2), (Just 33, 2)]
        negTasks = ["* false", "sdasda", "3 * +", "5 4 + *", "d 7 8 + *", "adas ewf +"]
        negAns = [(Nothing, 0),(Nothing, 0),(Nothing, 0),(Nothing, 1),(Nothing, 0),(Nothing, 0)]
        dictFunc "a" = Just 1
        dictFunc "b" = Just 4
        dictFunc "c" = Just 7
        dictFunc _ = Nothing




-- Функция main.

askVarVals :: [String] -> WriterT [(String, Int)] IO ()
askVarVals [] = return ()
askVarVals (v:vs) = do
    liftIO $ putStr $ "Введите значение для "++v++": "
    val <- liftIO $ getLine
    tell [(v, read val)]
    askVarVals vs

main = do
    putStr "Введите выражние для вычисления: "
    expr <- words `liftM` getLine
    let vars = filter (not . isCorrectSymbol (\_ -> Nothing)) expr
    varVals <- execWriterT $ askVarVals vars
    let (val, acts) = newEvalRPN (flip lookup varVals) expr
    if isNothing val
    then putStrLn "Выражние не удалось решить."
    else do
        mapM_ putStr ["Выражение \"", unwords expr, "\" решено.\n"]
        mapM_ putStr ["Ответ: ",show $ fromJust val," (получен за ", show acts, " шагов).\n"]

