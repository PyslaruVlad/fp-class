-- 1.1
-- Написать функцию, которая разбивает промежуток времени в секундах на часы, минуты и секунды.
-- Результат возвращать в виде кортежа из трёх элементов. Реализовать также обратное преобразование.
sec2hms :: Int -> (Int, Int, Int)
sec2hms time = (\ (h, s) -> (h, time `div` 60 - h * 60, s)) $ (time `div` 3600, time `mod` 60)

hms2sec :: (Int, Int, Int) -> Int
hms2sec (h, m, s) = 3600 * h + 60 * m + s

-- Реализовать с помощью hms2sec (здесь параметры заданы по отдельности)
hms2sec' :: Int -> Int -> Int -> Int
hms2sec' h m s = 3600 * h + 60 * m + s

-- должно быть True
test1 = and $ map (\x -> x == hms2sec (sec2hms x)) [1,10..10000]

-- 1.2
-- Написать функции, вычисляющие
-- а) длину отрезка по координатам его концов;
-- б) периметр и площадь треугольника по координатам вершин.

type Point = (Double, Double)

distance :: Point -> Point -> Double
distance (x1, y1) (x2, y2) = sqrt((x1 - x2)^2 + (y1-y2)^2)

triangle :: Point -> Point -> Point -> (Double, Double)
triangle p1 p2 p3 = (p, s)
  where
    p = distance p1 p2 + distance p2 p3 + distance p3 p1
    s = 0.5 * (fst vec12 * snd vec13 - snd vec12 * fst vec13)
		where
			vec12 = (fst p2 - fst p1, snd p2 - snd p1)
			vec13 = (fst p3 - fst p1, snd p3 - snd p1)

-- Во всех следующих заданиях использование стандартных функций обработки списков не допускается.
-- Все решения должны реализовываться рекурсивными функциями.

-- 2.1
-- Определить рекурсивную функцию, определяющую количество чётных элементов списка
nEven :: Integral a => [a] -> Int
nEven [] = 0
nEven (x:xs)
	| x `mod` 2 == 0	= 1 + (nEven xs)
	| otherwise 		= (nEven xs)

-- 2.2
-- Увеличить все элементы заданного списка в два раза.
-- Указание: в решении может понадобиться операция конструирования списка:
-- > 1 : [2,3,4]
--   [1,2,3,4]
doubleElems :: Num a => [a] -> [a]
doubleElems [] = []
doubleElems (x:xs) = (x * 2) : doubleElems xs

-- 2.3
-- Дан список целых чисел. Сформировать новый список, содержащий только нечетные элементы исходного.
fltOdd :: Integral a => [a] -> [a]
fltOdd [] = []
fltOdd (x:xs) 
	| x `mod` 2 == 0	= fltOdd xs
	| otherwise			= x : fltOdd xs

-- 2.4
-- Написать следующие функции обработки списков:
-- а) удалить все отрицательные элементы;
delNeg :: [Integer] -> [Integer]
delNeg [] = []
delNeg (x:xs)
	| x < 0			= delNeg xs
	| otherwise		= x : delNeg xs

-- б) увеличить элементы с чётными значениями в два раза;
doubleEven :: (Integral a, Num a) => [a] -> [a]
doubleEven [] = []
doubleEven (x:xs) 
	| x `mod` 2 == 0	= (x * 2) : doubleEven xs
	| otherwise			= x : doubleEven xs

-- в) переставить местами чётные и нечётные по порядку следования элементы
--    (для списков нечётной длины отбрасывать последний элемент).
elemsPermutation :: [a] -> [a]
elemsPermutation [] = []
elemsPermutation [x] = []
elemsPermutation (x:y:xs) = y : x : elemsPermutation xs

-- 2.5 
-- Даны два списка целых чисел. Сформировать список, каждый элемент которого равен сумме
-- соответствующих   элементов исходных списков. Предусмотреть ситуацию списков разной длины.
combine_plus :: [Integer] -> [Integer] -> [Integer]
combine_plus [] ys = ys
combine_plus xs [] = xs
combine_plus (x:xs) (y:ys) = (x+y) : combine_plus xs ys

-- 2.6
-- Даны два списка. Сформировать новый список, содержащий пары из соответствующих элементов
-- исходных списков. Хвост более длинного списка отбросить.
combine_pairs :: [a] -> [b] -> [(a,b)]
combine_pairs [] ys = []
combine_pairs xs [] = []
combine_pairs (x:xs) (y:ys) = (x, y) : combine_pairs xs ys

-- 2.7
-- Написать функции, которые по заданному n возвращают список, состоящий из n первых натуральных чисел
-- а) в порядке убывания;
nNatInDescOrder :: Int -> [Int]
nNatInDescOrder 0 = []
nNatInDescOrder n = n : nNatInDescOrder (n-1)

-- б) в порядке возрастания.
nNatInAscOrder :: Int -> [Int]
nNatInAscOrder 0 = []
nNatInAscOrder n = (nNatInAscOrder (n-1)) ++ [n]
-- (nNatInAscOrder (n-1)) : n - не работает :( 
-- мне кажется из-за того, что рекурсивная часть потенциально бесконечна???

-- 2.8
-- Дан элемент типа a и список [a]. Вставить между всеми элементами списка заданный элемент.
insertSome :: a -> [a] -> [a]
insertSome _ [a] = [a]
insertSome s (x:y:xs) = x : s : insertSome s (y:xs)

-- 2.9
-- Написать функцию, которая разбивает список на два подсписка: элементы из начала списка,
-- совпадающие с первым элементом, и все остальные элементы, например:
-- [1,1,1,2,3,1] -> ([1,1,1], [2,3,1]).
separateBegining :: Eq a => [a] -> ([a], [a])
separateBegining [] 		= ([],[])
separateBegining (x:xs) = sepBeg' ([x], xs)
	where
		sepBeg' (s, []) = (s, [])
		sepBeg' ((x:xs), (y:ys))
			| x == y		= sepBeg' ((y:x:xs), ys)
			| otherwise	= ((x:xs), (y:ys))


--3
-- Даны типовые аннотации функций. Попытайтесь догадаться, что они делают, и напишите их
-- рекурсивные реализации (если вы можете предложить несколько вариантов, реализуйте все):
-- а) [a] -> Int -> a
elementAtIndex :: [a] -> Int -> a
elementAtIndex [] p = error "пустой массив / выход за границы массива"
elementAtIndex (x:xs) i
	| i < 0		= error "индекс не может быть меньше нуля"
	| i == 0		= x
	| otherwise = elementAtIndex xs (i-1)

-- б) Eq a => [a] -> a -> Bool
containsElement :: Eq a => [a] -> a -> Bool
containsElement [] _ = False
containsElement (x:xs) e = (x==e) || containsElement xs e

-- в) [a] -> Int -> [a]
takeTheHead :: [a] -> Int -> [a]
takeTheHead [] _ = []
takeTheHead xs 0 = []
takeTheHead (x:xs) n = x : takeTheHead xs (n-1)

removeElement :: [a] -> Int -> [a]
removeElement [] _ = []
removeElement (x:xs) 0 = xs
removeElement (x:xs) i = x : removeElement xs (i-1)

-- г) a -> Int -> [a]
elemRepeat :: a -> Int -> [a]
elemRepeat e 0 = []
elemRepeat e n = e : elemRepeat e (n-1)

-- д) [a] -> [a] -> [a]
concatLists :: [a] -> [a] -> [a]
concatLists l [] = l
concatLists l (x:xs) = concatLists (l ++ [x]) xs

mixUpLists :: [a] -> [a] -> [a]
mixUpLists [] x = x
mixUpLists x [] = x
mixUpLists (x:xs) (y:ys) = x : y : mixUpLists xs ys

-- е) Eq a => [a] -> [[a]]
sortInEqualGroups :: Eq a => [a] -> [[a]]
sortInEqualGroups [] = []
sortInEqualGroups (x:xs) = (x : filterWith (x ==) xs) : (sortInEqualGroups $ filterWith (x /=) xs)
	where
		filterWith _ [] = []
		filterWith pred (x:xs)
			| pred x		= x : filterWith pred xs
			| otherwise	= filterWith pred xs


-- ж) [a] -> [(Int, a)]
-- Вспомогательная функция
enumList :: (Int -> Int) -> Int -> [a] -> [(Int, a)]
enumList _ _ [] = []
enumList f i (x:xs) = (i, x) : enumList f (f i) xs 

enumerateInAscOrder :: [a] -> [(Int, a)]
enumerateInAscOrder x = enumList (+1) 1 x

enumerateInDescOrder :: [a] -> [(Int, a)]
enumerateInDescOrder x = enumList (subtract 1) (length x) x

-- з) Eq a => [a] -> [a]
takeUniqe :: Eq a => [a] -> [a]
takeUniqe [] = []
takeUniqe (x:xs) = x : takeUniqe (filterWith (x /=) xs)
	where
		filterWith _ [] = []
		filterWith pred (y:ys)
			| pred y		= y : filterWith pred ys
			| otherwise	= filterWith pred ys
