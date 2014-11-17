import Control.Monad
import Data.List
{-
  Следующие типы задают множество состояний, алфавит и функцию переходов
  недетерминированного конечного автомата (НКА).
-}
type Alphabet = [Char]
type State = Int
type States = [State]
type AcceptingStates = [State]
type InitialState = State
type TransitionFunction = State -> Char -> States
type NFA = (Alphabet, States, InitialState, TransitionFunction, AcceptingStates)

-- пример НКА
nfa_ex :: NFA
nfa_ex = (['0','1'], [1, 2], 1, tf, [2])
  where
    tf 1 '0' = [1]
    tf 1 '1' = [1, 2]

-- Напишите функцию, определяющую, корректно ли задан НКА
isCorrect :: NFA -> Bool
isCorrect = undefined

-- в дальнейшем может пригодиться функция whileM,
-- которая собирает результаты в список, пока истинно
-- заданное условие
whileM :: m Bool -> m a -> m [a]
whileM = undefined

-- Напишите функцию, определяющую, допускает ли НКА заданное слово 
accept :: NFA -> String -> Bool
accept (alph, states, init, transFunc, fin) str = isInAlphabet && admitsStr
    where
        isInAlphabet = str `intersect` alph  == str
        transit states c = concat $ transFunc `liftM` states `ap` [c]
        endStates =  foldl transit [init] str
        admitsStr = endStates `intersect` fin  /= []
{-
    Просто необходимый комментарий. Я не учился на мехмате, но читал по книжкам.
    Поэтому некоторые детали могу упустить. Так вот:
    Книжки гласят, что атвомат должен прочесть строку полностью (!) и тогда если одно
    из его состояний будет финальным, то только тогда можно говорить, что строку он принял. 
    Иначе говоря, он должен знать что делать при любых комбинациях символов и состояний.
    Отсюда необходимо, чтобы поведение автомата даже в финальном состоянии 
    было определено. А это не выполняется с вашим примером nfa_ex. После перехода в финальное
    сосостояние он больше не может читать символы. Я немного в растерянности, но сделал так
    как было в книжках.
-}

-- Постройте ещё как минимум три примера НКА
nfa1 :: NFA
nfa1 = ("12", [0,1,2], 0, tf, [2]) -- Распознает строки с двумя подряд идущими '1'
    where
        tf 0 '1' = [1]
        tf 0 '2' = [0]
        tf 1 '1' = [2]
        tf 1 '2' = [0]
        tf 2 '1' = [2]
        tf 2 '2' = [2]

check_nfa1 = positive && not negative
    where
        positive = and $ (accept nfa2) `liftM` ["2112", "1211", "21212112212"]
        negative = or $ (accept nfa2) `liftM` ["21212", "12121", "22221221", "abcd"]

nfa2 :: NFA
nfa2 = ("123", [0..4], 0, tf, [4]) -- Принимает строки, которые оканчиваются на символ
    where                          -- который ранее уже встречался в строке.
        tf 0 '1' = [0,1]
        tf 1 '1' = [1,4]
        tf 2 '1' = [2]
        tf 3 '1' = [3]
        tf 4 '1' = []
        tf 0 '2' = [0,2]
        tf 1 '2' = [1]
        tf 2 '2' = [2,4]
        tf 3 '2' = [3]
        tf 4 '2' = []
        tf 0 '3' = [0,3]
        tf 1 '3' = [1]
        tf 2 '3' = [2]
        tf 3 '3' = [3,4]
        tf 4 '3' = []
        
check_nfa2 = positive && not negative
    where
        positive = and $ (accept nfa2) `liftM` ["121", "3213", "111", "321123"]
        negative = or $ (accept nfa2) `liftM` ["31112", "123", "11113", "abcd"]

nfa3 :: NFA
nfa3 = ("12", [0,1,2], 0, tf, [2]) -- Принимает строки у которых на конце две '1'
    where
        tf 0 '1' = [0,1]
        tf 1 '1' = [2]
        tf 2 '1' = []
        tf 0 '2' = [0]
        tf 1 '2' = []
        tf 2 '2' = []

check_nfa3 = positive && not negative
    where
        positive = and $ (accept nfa2) `liftM` ["1211", "212211", "11", "2121112211"]
        negative = or $ (accept nfa2) `liftM` ["1", "21", "112","1121", "abcd"]

{-
  Распределите заданные строки в соответствии с распознающими
  их НКА (одна строка может попасть в несколько групп).
-}

classify :: [NFA] -> [String] -> [(NFA, [String])]
classify nfas strs = formTuple `liftM` nfas
    where
        formTuple a = (a, accept a `liftM` strs `deleteFalse` strs)
        appendIf (c:cs, acc) e = if c then (cs, e:acc) else (cs, acc)
        deleteFalse conds els = reverse $ snd $ foldl appendIf (conds, []) els

check_classify = ([ans1, ans2, ans3] ==) $ snd $ unzip $ classify nfas strs
    where
        nfas = [nfa1, nfa2, nfa3]
        strs = ["121", "3213", "111", "1", "21", "112", "2112", "1211"
               ,"31112", "123", "abcd", "212211", "11","21212", "12121"]
        ans1 = ["111", "112", "2112", "1211", "212211", "11"]
        ans2 = ["121", "3213", "111", "2112", "1211","212211", "11","21212", "12121"]
        ans3 = ["111","1211", "212211", "11"]
