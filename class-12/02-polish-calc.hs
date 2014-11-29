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
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Writer
import Control.Monad.Reader

import Data.Maybe

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

incorrectCondition :: Stack -> String -> Bool
incorrectCondition stack c = isOperation c && length stack < 2
    where 
        isOperation e = e `elem` operations
        operations = ["+","*"]

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
    if incorrectCondition stack c
    then mzero
    else lift $ performStep c

newEvalRPN :: (String -> Maybe Int) -> [String] -> (Maybe Int, Int)
newEvalRPN lookupFunc = checkMaybe . runAll . runMaybeT . mapM processSymbol
    where 
        checkMaybe ((Just _, c), stack) = (Just $ head stack, getSum c)
        checkMaybe ((Nothing, c), _) = (Nothing, getSum c)
        runAll = (flip runState []) . runWriterT . (flip runReaderT lookupFunc)





-- Тесты.


testRPN = positiveTest && negativeTest
    where
        positiveTest = newEvalRPN dictFunc (words "3 2 4 + *") == (Just 32, 2)
        negativeTest = newEvalRPN dictFunc (words "3 2 4 + * *") == (Nothing, 2)
        dictFunc = \_ -> Just 4

