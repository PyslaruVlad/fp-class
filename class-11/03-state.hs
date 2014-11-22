import Control.Monad
import Control.Monad.State

enqueue :: a -> State [a] ()
enqueue a = get >>= \ ls -> put (ls++[a])

dequeue :: State [a] a
dequeue = get >>= \ (a:ls) -> put ls >> return a

data Operation = Enq Int | Deq

apply :: Operation -> State [Int] ()
apply (Enq n) = enqueue n
apply Deq = dequeue >> return () -- На результат смотрим по оставшемуся "внутри" списку.

applyToList :: [Operation] -> [Int] -> [Int]
applyToList ops ls = execState (mapM apply ops) ls

-- Проверочка.

checkQueue :: Bool
checkQueue = map finList [1,2,3] == map ans [1,2,3]
    where
        finList 1 = applyToList [Enq 1, Enq 2, Deq] [3,4]
        finList 2 = applyToList [Enq 7, Enq 6, Deq, Enq 5, Deq] [7,8]
        finList 3 = applyToList [Enq 5, Deq, Enq 8, Enq 9, Deq] [10]
        ans 1 = [4,1,2]
        ans 2 = [7,6,5]
        ans 3 = [8,9]

