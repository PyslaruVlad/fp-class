{-
Явная рекурсия в решениях хотя и допускается, но не приветствуется. Старайтесь обходиться стандартными
функциями, используя при этом создание функций «на лету». Пытайтесь максимально упростить уже написанные
решения, применяя подходящие функции из модуля Data.List и любых других модулей. Перед выполнением заданий
изучите примеры из лекции по функциям высшего порядка. 
-}

{-
 1. Простейшие задачи на применение функций map и filter.
 1.1 Преобразовать данный список целых чисел следующим образом:
  a) увеличить все его элементы в два раза;
  b) увеличить все его элементы с четными значениями в два раза;
  с) обнулить все его элементы с нечетными значениями;
  d) удалить из него элементы, большие заданного числа k;
  e) отфильтровать его, оставив в списке только отрицательные числа;
  f) удалить из него все положительные чётные числа.
-}

f11a :: Integral a => [a] -> [a]
f11a = map (*2)

f11b :: Integral a => [a] -> [a]
f11b = map (\ x -> if even x then 2 * x else x)

f11c :: Integral a => [a] -> [a]
f11c= map (\ x -> if even x then x else 0)

f11d :: (Num a, Ord a) => a -> [a] -> [a]
f11d k = filter (\ x -> x <= k)

f11e :: (Num a, Ord a) => [a] -> [a]
f11e = f11d 0

f11f :: (Integral a, Ord a) => [a] -> [a]
f11f = filter (\ x -> (x < 0) || (x `mod` 2 /= 0))

{-
 1.2 Дан список декартовых координат точек на плоскости (пар вещественных чисел).
     Преобразовать его следующим образом:
  a) отфильтровать список так, чтобы в нём остались точки из заданной координатной четверти;
  b) преобразовать декартовы координаты в полярные.
-}

f12a :: (Ord a, Num a) => Integer -> [(a, a)] -> [(a, a)] 
f12a n = filter (\ (x, y) -> 		{-1-}(n == 1 && x > 0 && y > 0) || 
											{-2-}(n == 2 && x < 0 && y > 0) || 
											{-3-}(n == 3 && x < 0 && y < 0) || 
											{-4-}(n == 4 && x > 0 && y < 0))

f12b :: Floating a => [(a,a)] -> [(a,a)]
f12b = map (\ (x, y) -> (sqrt $ x^2 + y^2, atan y/x))

{-
 1.3 Дан список слов.
  a) Преобразовать все слова к верхнему регистру.
  b) Извлечь из него подсписок слов заданной длины.
  c) Извлечь из него подсписок слов, начинающихся с заданной буквы.
-}

f13a :: [String] -> [String]
f13a = map $ map (\ x -> toEnum $ fromEnum x - 32)

f13b :: Int -> [String] -> [String]
f13b n = filter (\ x -> (length x == n))

f13c :: Char-> [String] -> [String]
f13c l = filter (\ x -> (head x == l))

{-
2. Формирование числовых последовательностей (iterate).
 a) Список натуральных чисел, начиная с 0.
 b) Список чётных чисел.
 c) Список элементов последовательности: a0=1, an=(1+an-1)/2.
 d) Список символов английского алфавита.
 e) Список строк, представляющих n-значные двоичные числа.
-}

nats :: [Integer]
nats = iterate (+1) 0

evens :: [Integer]
evens = iterate (+2) 2

aSeq :: [Double]
aSeq = flip iterate 1 $ (\ x -> (1+x)/2)

engAlph :: [Char]
engAlph = fromAtoZ 'A' 'Z' ++ fromAtoZ 'a' 'z'
	where 
		fromAtoZ :: Char -> Char -> [Char]
		fromAtoZ a z = map toEnum $ takeWhile (<= fromEnum z) $ iterate (+1) $ fromEnum a

f2e :: Int -> [String]
f2e n = map complete $ takeWhile (\ x -> length x <= n) $ map toBin $ iterate (+1) 1
	where
		toBin :: Integral a => a -> String
		toBin 0 = []
		toBin k 
			| k `mod` 2	== 0		= toBin (div k 2) ++ ['0']
			| otherwise 			= toBin (div k 2) ++ ['1']
		complete :: String -> String
		complete xs
			| length xs < n		= complete $ '0':xs
			| otherwise				= xs

{-
3. Группировка списков.
  a) Дан список символов. Сгруппировать подряд идущие символы по принципу: цифры — не цифры — ...
  b) Дан список пар вещественных чисел (координат точек на плоскости). Сгруппировать подряд идущие
     координаты точек, лежащие в одной координатной четверти.
  c) Дан список и ненулевое натуральное число n. Разбить список на подсписки длиной n каждый.
     Последний подсписок может содержать менее n элементов.
  d) Дан список и ненулевые натуральные числа n и m. Разбить список на перекрывающиеся подсписки
     длиной n элементов со сдвигом относительно предыдущего подсписка на m элементов.
  e) Дан список. Определить длину самого длинного подсписка, содержащего подряд идущие одинаковые элементы.
-}


-- Вспомогательная функция.
divideInGroups :: (a -> a -> Bool) -> [a] -> [[a]]
divideInGroups _ [] = []
divideInGroups pred (x:xs) = [x : takeWhile (pred x) xs] ++ (divideInGroups pred $ dropWhile (pred x) xs)


f3a :: [Char] -> [[Char]]
f3a ss = map (map toEnum) $ divideInGroups pred $ map fromEnum ss
	where
		isNum n = n <= 57 && n >= 48
		pred x
			| isNum x		= isNum
			| otherwise		= not . isNum



f3b :: (Real a) => [(a, a)] -> [[(a, a)]]
f3b = divideInGroups pred
	where
		pred (x,y)
			| x > 0 && y > 0		= (\ (a, b) -> a > 0 && b > 0)
			| x < 0 && y > 0		= (\ (a, b) -> a < 0 && b > 0)
			| x < 0 && y < 0		= (\ (a, b) -> a < 0 && b < 0)
			| x > 0 && y < 0		= (\ (a, b) -> a > 0 && b < 0)
			| otherwise 			= (\ (a, b) -> False)


f3c :: Int -> [a] -> [[a]]
f3c _ [] = []
f3c n xs = [take n xs] ++ (f3c n $ drop n xs)


f3d :: [a] -> Int -> Int -> [[a]]
f3d [] _ _ = []
f3d xs n m = [take n xs] ++ (f3d (drop m xs) n m)

-- Должно быть True
test_f3d = f3d [1..10] 4 2 == [[1,2,3,4],[3,4,5,6],[5,6,7,8],[7,8,9,10],[9,10]]


f3e :: Eq a => [a] -> Int
f3e xs = findMax $ map length $ divideInGroups pred xs
	where
		pred a = (\ x -> x == a)
		findMax [] = 0
		findMax (x:xs) = max x $ findMax xs

{-
4. Разные задачи.
 a) Дан текст в виде строки символов, содержащий среди прочего числовые данные. Посчитать количество
    всех упоминающихся в тексте чисел.
 b) Найти сумму всех чисел Фибоначчи, удовлетворяющих заданному предикату, в указанном промежутке
    (например: все чётные от 1 до 106).
 c) Дана строка текста и число n. Сформировать список, содержащий n самых часто используемых
    в строке символов.
 d) Дан список чисел. Сформировать список локальных максимумов исходного списка. Локальным максимумом
    называется элемент, больший своих соседей.
 e) Дан список. Продублировать все его элементы.
-}

f4a :: String -> Int
f4a str = length $ filter isNum $ divInWords str
	where
		divInWords = divideInGroups pred
		pred x
			| x == ' '		= (\ y -> y == ' ')
			| otherwise		= (\ y -> y /= ' ')
		isNum [] 		= True
		isNum (c:cs)	= (fromEnum c <= 57 && fromEnum c >= 48) && isNum cs


f4b :: Int -> Int -> (Int -> Bool) -> Int
f4b max min pred = sum $ filter pred $ takeWhile (<=max) $ dropWhile (< min) fibs
	where
		fibs = 0 : 1 : zipWith (+) fibs (tail fibs)


f4c :: String -> Int -> String
f4c str n = (snd . unzip) $ take n $ sort $ addFreq str
	where
		addFreq [] = []
		addFreq (c:cs) = (freq c str, c) : (addFreq $ filter (/= c) cs)
		freq _ [] = 0
		freq c (cn:cs)
				| cn == c		= 1 + freq c cs
				| otherwise		= freq c cs
		sort [] = []
		sort ((x, y):xys) = sort (filter (\ (a, b) -> a > x) xys) ++ [(x,y)] 
									++ sort (filter (\ (a, b) -> a <= x) xys)


f4d :: (Ord a, Num a) => [a] -> [a]
f4d xs
	| length xs >= 2	= [max (xs !! 0) (xs !! 1)] ++ recStep xs
	| otherwise			= recStep xs
	where
		recStep :: (Ord a, Num a) => [a] -> [a]
		recStep [] 				= []
		recStep [x] 			= [x]
		recStep (x:y:[])		= if (x >= y) then [x] else [y]
		recStep (x:y:z:xs)	= (++) locMax $ recStep (y:z:xs)
			where
				locMax
					| y >= x && y >= z		= [y]
					| otherwise 				= []


f4e :: [a] -> [a]
f4e [] = []
f4e (x:xs) = x : x : f4e xs


