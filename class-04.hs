{-
  Все задачи в этом задании должны решаться исключительно с помощью свёрток.
  Явная рекурсия не допускается. Если в решении в качестве вспомогательной
  требуется стандартная функция обработки списков (помимо fold*, scan*), она
  также должна реализовываться свёрткой.

  Каждое решение должно сопровождаться тремя различными тестовыми примерами, которые при запуске
  возвращают True, например:

  f = undefined -- решение 
  f_test1 = f undefined == undefined -- тест 1
  f_test2 = f undefined == undefined -- тест 2
  f_test3 = f undefined == undefined -- тест 3
-}

{-
 1. Простейшие функции обработки списков
  a) Найти сумму чётных элементов списка с целочисленными элементами.
  b) Найти сумму и произведение элементов списка вещественных чисел.
  с) Найти среднее арифметическое элементов списка вещественных чисел (функцией length пользоваться нельзя,
     решение должно выполняться в один проход).
  d) Найти минимальный элемент списка.
  e) Найти наименьший нечётный элемент списка с целочисленными значениями (дополнительным параметром
     функции должно быть значение, возвращаемое по умолчанию).
-}


-- Некоторые основные функции переписанные с использование fold
reverse' = foldl (flip (:)) []
length' = foldl (\ acc x -> acc +1) 0
tail' (a:arr) = arr
head' (a:arr) = a
map' func = foldr (\ x acc -> (func x):acc) []





f1a :: [Int] -> Int
f1a = (foldl (+) 0) . (filter even)

f1a_test mi ma = f1a [mi..ma] == sum [n | n <- [mi..ma], even n]

f1a_test1 = f1a_test 1 10
f1a_test2 = f1a_test 100 1000
f1a_test3 = f1a_test 999 90099


f1b :: Real a => [a] -> (a, a)
f1b = foldl (\ (s, m) x -> (s+x, m*x)) (0,1) 

f1b_test1 = f1b [1..5] == (15, 120)
f1b_test2 = f1b [98..100] == (98 + 99 + 100, 98 * 99 * 100) 
f1b_test3 = f1b realList == (sum realList, product realList)
	where
        realList = [x/1000 | x <- [1..50000]]


f1c :: (Fractional a, Real a) => [a] -> a
f1c = (\ (s, q) -> s/q) . foldl (\ (s,q) x -> (s+x, q+1)) (0,0)

f1c_test1 = f1c [0..10] == 5.0
f1c_test2 = f1c [0..1000] == 500
f1c_test3 = f1c [1000..10000] == 55 * 100


f1d :: (Ord a, Num a) => [a] -> a
f1d = foldl1 min

f1d_test1 = f1d ([500..600] ++ [0] ++ [1000..100000]) == 0
f1d_test2 = f1d ([-5] ++ [-12] ++ [0]) == -12
f1d_test3 = f1d ([1] ++ [1] ++ [1]) == 1


f1e :: Int -> [Int] -> Int
f1e def = foldl (\ acc x -> if (x<acc && odd x) || (acc==def && odd x) then x else acc) def

f1e_test1 = f1e (-1) ([100..150]++[2]++[5]++[200..240]) == 5
f1e_test2 = f1e (-1) ([100] ++ [2] ++ [200] ++ [240]) == -1
f1e_test3 = f1e (333) ([100] ++ [3] ++ [200] ++ [240]) == 3


{-
 2. Свёртки, формирующие списки
  a) Сформировать список, содержащий каждый второй элемент исходного.
  b) Сформировать список, содержащий первые n элементов исходного.
  c) Сформировать список, содержащий последние n элементов исходного.
  d) Сформировать список, содержащий все элементы исходного списка, большие левого соседа.
  e) Сформировать список, содержащий все локальные минимумы исходного списка.
  f) Дана строка, содержащая слова, разделённые одним или несколькими пробелами. Сформировать
     список слов этой строки.
  g) Разбить список на непересекающиеся подсписки длиной n элементов.
  h) Разбить список на подсписки длиной n элементов с перекрывающейся частью в k элементов (k < n).
  k) Сформировать список, содержащий все начальные элементы списка, удовлетворяющие заданному предикату.
  l) Повторить каждый элемент списка заданное количество раз.
  m) Удалить из списка повторяющиеся подряд идущие элементы.
  n) Даны два списка одинаковой длины. Сформировать список, состоящий из результатов применения
     заданной функции двух аргументов к соответствующим элементам исходных списков.
-}

f2a :: [a] -> [a]
f2a = reverse' . snd . foldl foldFunc (False, [])
    where
        foldFunc (flag, acc) x
            | flag      = (not flag, x:acc)
            | otherwise = (not flag, acc)

f2a_test1 = f2a [1] == []
f2a_test2 = f2a [1..3] == [2]
f2a_test3 = f2a [1..10] == [2*n| n <- [1..5]]





f2b :: Int -> [a] -> [a]
f2b n = reverse' . snd . foldl (foldFunc) (0, [])
    where
        foldFunc (count, acc) x
            | count < n    = (count +1, x:acc)
            | otherwise    = (count, acc)

f2b_test1 = f2b 10 [1] == [1]
f2b_test2 = f2b 10 [1..20] == [1..10]
f2b_test3 = f2b 0 [1..100] == []





f2c :: Int -> [a] -> [a]
f2c n = snd . foldr (foldFunc) (0, [])
    where
        foldFunc x (count, acc)
            | count < n   = (count +1, x:acc)
            | otherwise   = (count, acc)

f2c_test1 = f2c 10 [1] == [1]
f2c_test2 = f2c 10 [1..20] == [11..20]
f2c_test3 = f2c 0 [1..100] == []





f2d :: Ord a => [a] -> [a]
f2d (x:xs) = reverse' $ snd $ foldl foldFunc (x, []) xs
    where
        foldFunc (lN, acc) a
            | a > lN    = (a, a:acc)
            | otherwise = (a, acc)
            
f2d_test1 = f2d [1..10] == [2..10]
f2d_test2 = f2d [10,9,8,9,7,5,6] == [9,6]
f2d_test3 = f2d [1] == []






-- Не учитывает краевых элементов. Если их учет необходим (из условия не совсем ясно)
-- то могу исправить.
f2e :: Ord a => [a] -> [a]
f2e (x1:x2:xs) = reverse' $ thrd' $ foldl foldFunc (x1, x2, []) xs
    where
        thrd' ( _, _, t) = t
        foldFunc (l, m, acc) r
            | r <  m || l < m   = (m, r, acc)
            | otherwise         = (m, r, m:acc)
f2e (x:xs) = []

f2e_test1 = f2e [9,7,8,6,7,5,6] == [7,6,5]
f2e_test2 = f2e [1,2] == []
f2e_test3 = f2e [3,5,6,7,1] == []






f2f :: String -> [String]
f2f = snd . foldr foldFunc ([], []) . (' ':)
    where
        foldFunc x (wrd, acc)
            | x /= ' '                     = (x : wrd, acc)
            | x == ' ' && (wrd /= "")      = ([], wrd : acc)
            | otherwise                    = ([], acc)
            
f2f_test1 = f2f "     " == []
f2f_test2 = f2f "   word1   word2   " == ["word1", "word2"]
f2f_test3 = f2f "word1    word2 word3" == ["word1", "word2", "word3"]






-- Если честно, не знаю что здесь оптимальнее - конкатенировать "++"
-- или реверсировать списки по многу раз.
f2g :: Int -> [a] -> [[a]]
f2g n = reverse' . snd . foldl foldFunc ([], [])
    where
        foldFunc (tuple, acc) x
            | (< (n-1)) $ length' tuple = (x : tuple, acc)
            | otherwise  = ([], reverse' (x:tuple) : acc)

f2g_test1 = f2g 3 [1..10] == [[1..3],[4..6],[7..9]]
f2g_test2 = f2g 10 [1..9] == []
f2g_test3 = f2g 5 [1..10] == [[1..5], [6..10]]




-- Да-да - здесь я использую функцию которая определена чуть ниже и может показаться, что позже,
-- но на самом деле я написал zipWith гораздо раньше f2h, потому как экспериментировал
-- с ней, а потом переписал ее в f2n и оставил только там, чтобы не загромождать текст.

f2h :: Int -> Int -> [a] -> [[a]]
f2h n k xs = reverse' $ snd $ foldl take (n-1-k, []) $ f2hN1
    where
        cutHead [] _ = []
        cutHead (a:acc) i = tail' a : (a:acc)
        nTails = foldl cutHead [xs] [2..n]
        zipFunc acc x = f2n (flip (:)) acc x
        f2hN1 = foldl zipFunc (f2g 1 $ head' nTails) $ tail' nTails
        take (count, acc) arr
            | count == n-1-k    = (0, arr:acc)
            | otherwise         = (count+1, acc)
    
f2h_test1 = f2h 3 2 [1..3] == [[1..3]]
f2h_test2 = f2h 3 1 [1..7] == [[1,2,3],[3,4,5],[5,6,7]]
f2h_test3 = f2h 4 3 [1..7] == [[1,2,3,4],[2,3,4,5],[3,4,5,6],[4,5,6,7]]





            
f2k :: (a -> Bool) -> [a] -> [a]
f2k pred = reverse' . snd . foldl foldFunc (True, [])
    where
        foldFunc (flag, acc) x
            | flag && pred x = (True, x:acc)
            | otherwise      = (False, acc)

f2k_test1 = f2k (\ x -> x == 3) [3,3,3,4,5,6] == [3,3,3]
f2k_test2 = f2k (\ x -> x == 5) [3,3,3,4,5,6] == []
f2k_test3 = f2k (\ x -> odd x) [3,3,3,4,5,6,7] == [3,3,3]






f2l :: Int -> [a] -> [a]
f2l n = foldr foldFunc []
    where
        dup m a = foldl (\ acc x -> a:acc) [] [0..m]
        foldFunc x acc = (dup n x) ++ acc
        
f2l_test1 = f2l 0 [1..5] == [1..5]
f2l_test2 = f2l 4 [1] == [1,1,1,1,1]
f2l_test3 = f2l n xs == foldr ((++) . repN) [] xs
    where
        n = 4
        xs = [1..123]
        repN = (replicate (n+1))





f2m :: Eq a => [a] -> [a]
f2m = reverse' . foldl foldFunc []
    where
        elem' b = foldl (\ acc c -> (c==b) || acc) False
        foldFunc acc a
            | elem' a acc   = acc
            | otherwise     = a:acc
            
f2m_test1 = f2m "aaabbbbbccccccddddd     " == "abcd "
f2m_test2 = f2m "abcdabcdbadcbadbcbacbbadbcba" == "abcd"
f2m_test3 = f2m "tyuuyt1234tyuy5tyuytutyu67" == "tyu1234567"





f2n :: (a -> b -> c) -> [a] -> [b] -> [c]
f2n f xs ys = reverse' $ snd $ foldl foldFunc (xs, []) ys
    where
        foldFunc ([], acc) t = ([], acc)
        foldFunc (p:ps, acc) t = (ps, (f p t) : acc)

f2n_test1 = f2n (+) [1..3] [4..1032] == [5,7,9]
f2n_test2 = f2n (\ a b -> (a,b)) [1..1000] [901..1000] == zip [1..100] [901..1000]
f2n_test3 = f2n (\ a b -> 's') [1..100] [1..100] == replicate 100 's'

{-
 3. Использование свёртки как носителя рекурсии (для запуска свёртки можно использовать список типа [1..n]).
  a) Найти сумму чисел от a до b.
  b) Найти сумму факториалов чисел от a до b (повторные вычисления факториалов не допускаются).
  с) Сформировать список из первых n чисел Фибоначчи.
  d) Пользуясь рядом Тейлора, вычислить значение синуса заданного числа x (использовать
     n слагаемых).
  e) Проверить, является ли заданное целое число простым.
-}

f3a :: (Enum a, Num a) => a -> a -> a
f3a a b = foldl (\ acc x -> x + acc) 0 [a..b]

f3a_test1 = f3a 1 5 == 15
f3a_test2 = f3a 5 1 == 0
f3a_test3 = f3a 1435 1532654 == sum [1435..1532654]






f3b :: (Enum a, Num a, Eq a) => a -> a -> a
f3b a b = snd $ foldl foldFunc (fact (a-1), 0) [a..b]
    where
        foldFunc (p,s) x = (p*x, p*x + s)
        fact 0 = 1
        fact x = x * fact (x-1)
   
f3b_test1 = f3b 5 1 == 0
f3b_test2 = f3b 5 34 == sum [f3b n n | n <- [5..34]]
f3b_test3 = f3b 50 100 == sum [f3b n n | n <- [50..100]]
f3b_test4 = f3b 1 1000 == sum [fact n | n <- [1..1000]]
    where
        fact 1 = 1  
        fact x = (x *) $ fact (x-1)






f3c :: (Enum a, Num a) => a -> [Integer]
f3c n = map' fst $ scanl foldFunc (0,1) [2..n]
    where
        foldFunc (prev, cur) x = (cur, cur + prev)
        
f3c_test1 = f3c 1 == [0]
f3c_test2 = f3c 3 == [0,1,1]
f3c_test3 = f3c 45 == take 45 fibs
    where
        fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
        




f3d :: (Integral b, Floating a) => b -> a -> a
f3d n a = snd $ foldl foldFunc (a, 0) [1..(2*n+1)]
    where
        foldFunc (p, s) x
            | odd x     = (p / fromIntegral x, s + p / fromIntegral x)
            | otherwise = (-p *a *a / fromIntegral x, s)

n = 10000
f3d_test1 = abs (f3d n pi) < 1/ fromIntegral n
f3d_test2 = abs (f3d n (pi/2) - 1) < 1/ fromIntegral n
f3d_test3 = abs (f3d n 0) < 1/ fromIntegral n
 
         
         
            
f3e :: Integral a => a -> Bool
f3e x = foldl (\ acc n -> (x `mod` n /=0) && acc) True [2..(x-1)]

f3e_test1 = f3e 2 == True
f3e_test2 = f3e 1 == True
f3e_test3 = map' f3e [41, 47, 53, 59] == [True, True, True, True]    

{-
 4. Решить задачу о поиске пути с максимальной суммой в треугольнике (см. лекцию 3) при условии,
   что необходимо дополнительно найти сам путь (к примеру, в виде закодированных направлений спуска:
   0 - влево, 1 - вправо). В решении допускается использование любых стандартных функций.
-}

downstep :: [Int]->[Int]->[Int]
downstep upper lower = zipWith (+) lower $ zipWith max (0:upper) (upper++[0])

answer :: [[Int]] -> Int
answer = maximum . foldl1 downstep

answer_test1 = answer [[3],[7,4],[2,4,6],[8,5,9,3]] == 23
answer_test2 = answer [[3],[7,4],[2,4,63],[8,5,9,3]] == 79
answer_test3 = answer [[3],[7,4],[2,4,63],[1888,5,9,3]] == 1900


-- Вспомогательная функция. Ищет индекс максимума в списке.
findMax (y:ys) = pickIndex $ foldl maxF (1,1,y) ys
    where
        pickIndex (c, i, m) = i
        maxF (c, i, p) n
            | p > n       = (c+1, i, p)
            | otherwise   = (c+1, c+1, n)


-- Прежде всего функция собирает состояния аккумулятора начиная с конца работы оригинального
-- алгоритма. Затем ищет индекс максимума в состоянии аккумулятора на последнем шаге (findMax).
-- После это запускается итерационная часть: алгоритм пробегает все состояния от конца к началу
-- и выбирает путь с наибольшим (из двух возможных) показателем пути от начала до данного 
-- итерационного шага. Тем самым алгоритм ищет как-бы обратный путь, по максимальным меткам.
-- Попутно записывая номера меток: при условии, что вершина, из которой начинает работу оригинальный
-- алгоритм смотрит вверх, 0 соответствет левой стороне треугольника и далее по возрастанию.
-- Также алгоритм следит за поворотами и записывает их. False означает поворот в сторону младших
-- индексов, True - в сторону больших.
f4a :: [[Int]] -> ([Bool],[Int])
f4a = foldr traceroute ([],[]) . scanl1 downstep
    where
        traceroute x ([], []) = ([], [(findMax x) -1])
        traceroute x (turns, (p:ways)) = ((p > nMI p x):turns, (nMI p x):p:ways)
        nMI p x
            | p-1 < 0           = p
            | p > length x -1   = p-1
            | x!!(p-1) <= x!!p  = p
            | otherwise         = p-1
        
        
f4a_test1 = f4a [[3],[7,4],[2,4,6],[8,5,9,3]] == ([False,True,True],[0,0,1,2])
f4a_test2 = f4a [[3],[7,4],[2,4,63],[8,5,9,3]] == ([True,True,False],[0,1,2,2])
f4a_test3 = f4a [[3],[7,400],[2,4,63],[1888,5,9,3]] == ([False, False, False],[0,0,0,0])

{-
 5. Пусть числовые матрицы представлены списками строк. Реализовать следующие функции:
  1) транспонирование матрицы;
  2) сумма двух матриц;
  3) произведение двух матриц.
-}
-- К сожалению понял, что решения должны быть строками а не массивами уже слишком поздно.
-- Однако поскольку сделал все довольно универсально, то определяю функции, которые должны быть
-- ответом к заданию через универсальные соответствующие варианты.
-- Поэтому после каждой функции на массивах привожу пример того, как они работали бы на строках.
-- Соответствующим функциям в имена дописываю "str" - они являются ответом.
-- Следующие функции необходимы для преобразований от одного представления к другому:

toMatr :: [String] -> [[Int]]
toMatr = map' ((map' read) . f2f)

fromMatr :: Show a => [[a]] -> [String]
fromMatr = map' (insertSp . (map' show))
    where
        insertSp = tail' . foldr (\ x acc -> " "++x++acc) []




f51 :: [[a]] -> [[a]]
f51 (x:xs) = map' reverse' $ foldl (f2n $ flip (:)) (f2g 1 x) xs
        
f51_test1 = f51 ["abc", ",,,", "123"] == ["a,1","b,2","c,3"]
f51_test2 = f51 [[1,2,3],[1,2,3],[1,2,3]] == [[1,1,1],[2,2,2],[3,3,3]]
f51_test3 = f51 ["ab", "xy"] == ["ax", "by"]


f51str :: [String] -> [String]
f51str = fromMatr . f51 . toMatr
f51str_test1 = f51str ["5 7","1 2"] == ["5 1","7 2"]
f51str_test2 = f51str ["1 2 3","1 2 3","1 2 3"] == ["1 1 1","2 2 2","3 3 3"]
f51str_test3 = f51str ["1 0 0","0 1 0","0 0 1"] == ["1 0 0","0 1 0","0 0 1"]







f52 :: (a -> a -> b) -> [[a]] -> [[a]] -> [[b]]
f52 addo = f2n (f2n addo) 

f52_test1 = f52 ((++).(++"+")) matr1 matr2  == [["a1+x1","b1+y1"],["a2+x2","b2+y2"]]
    where
        matr1 = [["a1", "b1"], ["a2","b2"]]
        matr2 = [["x1","y1"], ["x2", "y2"]]
f52_test2 = f52 (+) [[1,1],[1,1]] [[1,1],[2,3]] == [[2,2],[3,4]]
f52_test3 = f52 (+) matr1 matr2 == sumMatr
    where
        matr1 = [[200,-5, 45, 0],[0,1,-1,0],[45,34,67,73],[1,1,-84,-45]]
        matr2 = [[300, 5, 55, 10],[1,0,2,1],[55,66,33,27],[1,1,-84,-45]]
        sumMatr = [[500,0,100,10],[1,1,1,1],[100,100,100,100],[2,2,-168,-90]]
       
        
f52str :: [String] -> [String] -> [String]
f52str xs ys = fromMatr $ f52 (+) (toMatr xs) (toMatr ys)
f52str_test1 = f52str ["1 -11","1 1"] ["1 1","2 3"] == ["2 -10", "3 4"]
f52str_test2 = f52str ["0 0","45 15"] ["3324 656","15 45"] == ["3324 656", "60 60"]
f52str_test3 = f52str matr1 matr2 == sumMatr
    where
        matr1 = ["200 -5 45 0","0 1 -1 0","45 34 67 73","1 1 -84 -45"]
        matr2 = ["300 5 55 10","1 0 2 1","55 66 33 27","1 1 -84 -45"]
        sumMatr = ["500 0 100 10","1 1 1 1","100 100 100 100","2 2 -168 -90"]








f53 :: (b -> b -> b) -> (a -> a -> b) -> [[a]] -> [[a]] -> [[b]]
f53 addo multo xs ys = foldr multi [] $ xs
    where
        sumo = foldl1 (\ acc x -> addo acc x)
        multi vec1 acc =  getStr vec1 (f51 ys) : acc
        getStr v1 m = foldr vecPrd [] m
            where
                vecPrd v2 acc = (sumo $ f2n multo v1 v2) : acc
                
f53_test1 = f53 (+) (*) [[1,0],[0,1]] [[3,4],[4,5]] == [[3,4],[4,5]]
f53_test2 = f53 (+) (*) matra matrb == [[-6,43],[-4,51/2]]
    where
        matra = [[2,1,4],[-4, 0.5, 3]]
        matrb = [[0,1],[-2,5],[-1,9]]
f53_test3 = f53 ((++).(++"+")) ((++).(++"*")) m1 m2 == m3
    where
         m1 = [["a1","b1"], ["a2","b2"]]
         m2 = [["x1","y1"], ["x2", "y2"]]
         m3 = [["a1*x1+b1*x2","a1*y1+b1*y2"],["a2*x1+b2*x2","a2*y1+b2*y2"]]
        
        
        
f53str :: [String] -> [String] -> [String]
f53str xs ys = fromMatr $ f53 (+) (*) (toMatr xs) (toMatr ys)
f53str_test1 = f53str matra matrb == ["8 25 27","23 -1 -1","13 18 14"]
    where
        matra = ["3 4 2 5","0 -1 3 2","1 2 3 0"]
        matrb = ["1 2 3","-3 5 4", "6 2 1", "1 -1 0"]
f53str_test2 =  f53str ["1 0", "0 1"] ["3 4", "4 5"] == ["3 4","4 5"]
f53str_test3 =  f53str ["0 0", "0 0"] ["3 4", "4 5"] == ["0 0","0 0"]

{-
 6. Реализовать левую свёртку, пользуясь правой. Проанализировать поведение собственной реализации
  на бесконечных списках и сравнить его с поведением оригинальной foldl.
-}

myFoldl func acc arr = foldr (flip func) acc $ reverse' arr

newF2b :: Int -> [a] -> [a]
newF2b n = reverse' . snd . myFoldl (foldFunc) (0, [])
    where
        foldFunc (count, acc) x
            | count < n    = (count +1, x:acc)
            | otherwise    = (count, acc)

newF2b_test1 = newF2b 10 [1] == [1]
newF2b_test2 = newF2b 10 [4..20] == [4..13]
newF2b_test3 = newF2b 0 [1..100] == []

-- После проверки всех трех вариантов можно сказать, что свертка реализована правильно
-- (хотя это наверное можно доказаться как-нибудь по-другому). В данном случае необходимо
-- было соблюсти правильный порядок обработки списка и результат показывает, что он
-- был выполнен. 

-- Что касается бесконченого списка, то достаточно взглянуть на то, как формируется
-- список для правой свертки внутри новой левой - обращается вспять - конец списка 
-- меняется местом с началом, что мне кажется "немного" затруднительно 
-- с бесконечными списками, у которых (надо же) нет конца.

-- К примеру можно попробовать вызвать вот эту функцию:
unlimitedSeq = take 6 $ myFoldl (\ acc x -> acc) [1..10] [1..]
-- То есть даже в таком простейшем случае функция не возрвращает никакого результата.
-- Хотя тут вроде бы все понятно - функция не меняет своего акумулятора и могла бы сразу
-- вернуть первые его 6 членов. НО! Удивило меня то, что при вызове такой же функции 
-- с оригинальной сверткой внутри - ничего не изменилось. 

limitedSeq = take 6 $ foldl (\ acc x -> acc) [1..10] [1..]
-- (я так и не дождался конца ее работы)

-- Мне кажется это из-за того, что функция не может "догадаться" что 
-- произойдет с ее аккумулятором во время "пробежки" по списку. И поэтому прежде 
-- чем вернуть значение аккумулятора, она в любом случае должна пройти все значения списка.
