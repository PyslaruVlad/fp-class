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

checkQueue = [4,1,2] == finList
    where
        finList = applyToList [Enq 1, Enq 2, Deq] [3,4]

