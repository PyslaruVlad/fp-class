-- IT314. Функциональное программирование
-- Занятие 1

-- 1) Функция без параметров (= константа)

hello :: String -- типовая аннотация (сигнатура)
hello = "Hello, world"

{-
  Запустите интерпретатор ghci (из каталога, в котором находится этот файл),
  загрузите этот файл:

> :load class-01

  и вызовите функцию hello:

> hello

-}

-- 2) Объявление функций

-- тип функции: Два параметра типа Double и результат того же типа
avg :: Double -> Double -> Double
avg a b = (a + b)/2

{-

  Пример вызова (передача параметров через пробел,
  пробел -- это операция вызова функции (применение)!):

> avg 5 9
7.0

  Функция может вызываться инфиксно:

> 5 `avg` 9
7.0

  а) Вычислите в ghci среднее арифметическое следующих пар чисел: 332 и 723, 34.34 и 93.27.
     Впишите ответы:
	527
	63.805

  б) Напишите функцию avg3, вычисляющую среднее арифметическое трёх заданных чисел.
     Проверьте результаты её работы на двух тройках чисел.

-}

avg3 :: Double -> Double -> Double -> Double
avg3 a b c = (a + b + c) / 3

{-
   После определения функции avg3 этот файл следует перезагрузить. Для этого в ghci необходимо выполнить
   команду :reload (или :r).

   Результаты проверки:

   avg3 1 2 3 = 2.0

-}

-- 3) Выражения

{-
   Вычислите и сохраните в этом файле значения следующих выражений,
   обращая внимание на обозначения и приоритеты операций, стандартные функции,
   расстановку скобок:

    2 + 3 		= 5
    mod 10 4 		= 2
    10 `mod` 4 		= 2
    True && 5 <	10 	= True
    5 < 7 || 10 > 3 	= True
    sqrt (-2) 		= NaN
    sqrt (sqrt 16) 	= 2.0
    let x = 4 in (sin x)^2 + (cos x)^2
    x			= 1.0
    7^(-1) 		
    error "AAAA!!!!"
    12345^54321 = Very Big Number
    2 < 3 || 9999954321^99912345 > 12345^54321 = True

-}

-- 4) Типы

{-
  Тип выражения можно узнать, воспользовавшись командой интерпретатора :t, например:

> :t 'a'
'a' :: Char
> :t 1
1 :: Num a => a

  Запись "1 :: Num a => a" означает, что выражение "1" имеет тип "a", где "a" принадлежит
  классу типов Num (имеет экземпляр класса типов Num, является числовым типом).

  Определите и сохраните в этом файле типы следующих выражений:
   5		-	5 :: Num a => a
   5.0		-	5.0 :: Fractional a => a
   sqrt 4	-	sqrt 4 :: Floating a => a
   sqrt 4.0	-	sqrt 4.0 :: Floating a => a
   2+3		-	2+3 :: Num a => a
   5 < 7	-	5<7 :: Bool
   if 2 > 3 then 7 else 5 -	if 2>3 then 7 else 5 :: Num a => a
   5 > 6 && False	-	5>6 && False :: Bool	

   Команда ":set +t" включает режим, при котором печатается тип каждого вычисляемого выражения.
   Команда ":set +s" включает режим, при котором печатается время вычисления каждого выражения.

-}

-- 5) Объявление функций (2)

-- а) Удвоение значения заданного числа
-- (типовая аннотация здесь означает, что функция принимает один параметр типа a и возвращает значение
--  типа a, причём тип a принадлежит классу типов Num)
double :: Num a => a -> a
double a = 2 * a

-- б) Утроение заданного числа
--    (типовую аннотацию и образцы параметров следует написать самостоятельно)
triple :: Num a => a -> a
triple a = 3 * a

-- в) Определение наибольшего из трёх заданных целых чисел (можно воспользоваться стандартной
--    двухаргументной функцией max).
max3 :: Ord a => a -> a -> a -> a
max3 a b c = max a (max b c)

{-
  Проверка:
> max3 87 34 209
209
> max3 22 28 30
30
> max3 12 25 (-7)
25

-}

-- г) Функция, возвращающая True тогда и только тогда, когда оба ее аргумента равны True
-- (пользоваться стандартными логическими операциями не следует, обратите внимание на
--  образцы параметров функции, последняя строка -- "во всех остальных случаях").
bothTrue :: Bool -> Bool -> Bool
bothTrue True True = True
bothTrue _  _ = False


-- д) Функция, возвращающая True, если только один из её аргументов равен True,
-- и False в противном случае (пользоваться стандартными логическими операциями не следует).
oneTrue :: Bool -> Bool -> Bool
oneTrue True False = True
oneTrue False True = True
oneTrue _ _ = False


-- е) Дана температура в градусах Фаренгейта. Вычислить соответствующую температуру
-- в градусах Цельсия.
f2c :: Double -> Double
f2c tf = (tf -32) * 5 / 9

{-
   ж) Найти наибольший общий делитель двух целых чисел, пользуясь
      алгоритмом Евклида (псевдокод):
      НОД(a, 0) = a.
      НОД(a, b) = НОД(b, a mod b), если b ≠ 0; 
-}
-- gcd' :: ???
gcd' :: Integral a => a -> a -> a
gcd' a 0 = a
gcd' a b = gcd' b (mod a b) 

-- з) Функция, возвращающая название дня недели по его номеру (от 1 до 7),
--    если номер неправильный, генерируется исключение (функция error).
--    В реализации следует пользоваться сопоставлением с образцами.
dayOfWeek :: Int -> String
dayOfWeek 1 = "Mon"
dayOfWeek 2 = "Tue"
dayOfWeek 3 = "Wed"
dayOfWeek 4 = "Thu"
dayOfWeek 5 = "Fri"
dayOfWeek 6 = "Sat"
dayOfWeek 7 = "Sun"
dayOfWeek _ = error "Number should be in range of 1 to 7"


-- Далее типовые аннотации, если их нет, следует писать самостоятельно.

-- 6) Условное определение функции

-- Пример.
-- Определение знака числа (-1, 0, 1). Класс типов Ord определяет операции сравнения.
sign :: (Num a, Ord a) => a -> Int
sign a
   | a < 0 = -1
   | a == 0 = 0
   | otherwise = 1

{-
   а) Найти значение функции f(x), вычисляемое по правилу:
          −x,   если x ≤ 0,
	  x^2,  если 0 < x < 2,
          4,    если x ≥ 2.
-}

eval_f :: (Ord a, Num a) => a -> a
eval_f x 
	| x <= 0 		= -x
	| 0 < x && x < 2 	= x^2
	| x >= 2		= 4

-- б) Написать функцию, возвращающую текстовую характеристику ("hot", "warm", "cool", "cold")
-- по заданному значению температуры в градусах Цельсия.
describeTemperature :: Double -> String
describeTemperature t
	| t < 20	=	"cold"
	| t < 25	=	"cool"
	| t < 35 	= 	"warm"
	| otherwise 	= 	"hot"

{- 
   в) (*) Дан список температур в градусах Фаренгейта. Вывести для каждого значения
    соответствующую текстовую характеристику.

  Решение:
> map (describeTemperature . f2c) [82, 94, 50, 65, 34]

  В этом решении с помощью операции (.) строится композиция (суперпозиция) функций
  и получившаяся функция применяется функцией map к каждому элементу списка.
-}

-- 7) Рекурсия

-- Пример. Вычислить сумму всех целых чисел от 1 до n (где n >= 1):

sum_n :: Int -> Int
sum_n 1 = 1
sum_n n
  | n > 1 = n + sum_n (n-1)
  | otherwise = error "n should be >= 1"

-- а) Вычислить сумму всех целых чисел от a до b включительно.

sum_ab :: Int -> Int -> Int
sum_ab a b
	| a < b 	= a + sum_ab (a+1) b
	| a == b 	= a
	| otherwise	= error "a must be less then b"

{-
   б) Числовая последовательность определяется следующим образом:
      a1 = 1, a2 = 2, a3 = 3, a_k = a_{k−1} + a_{k−2} − 2*a_{k−3}, k = 4, 5, ...
      Вычислить её n-й элемент.
-}

eval_a_n :: Int -> Int
eval_a_n n
	| n == 1	= 1
	| n == 2	= 2
	| n == 3	= 3
	| n < 1		= error "n must be more or equals 1"
	| otherwise 	= eval_a_n (n-1) + eval_a_n (n-2) - 2 * (eval_a_n (n-3))

-- в) Вычислить, пользуясь рекурсией, n-ю степень числа a (n - целое):

pow :: (Num a) => a -> Int -> a
pow a n
	| n == 0 	= 1
	| n == 1	= a
	| n < 0		= error "ну-ну"
	| otherwise	= a * (pow a (n-1))

-- г) Пользуясь ранее написанной функцией pow, вычислить сумму: 1^k + 2^k + ... + n^k.

sum_nk:: Int -> Int -> Int
sum_nk n k 
	| n < 1		= error "ну-ну"
	| n == 1	= 1
	| otherwise	= (pow n k) + (sum_nk (n-1) k)

-- д) Сумма факториалов чисел от 1 до n.

sum_fact :: (Num a, Ord a) => a -> a
sum_fact 1 = 1
sum_fact n = (fact n) + sum_fact (n-1)
  where
   	fact n 
			| n > 1		= n * (fact (n-1))
			| n == 1	= 1
			| otherwise 	= error "ну-ну"

-- е) Количество цифр целого числа

number_digits :: Int -> Int
number_digits 0 = 0
number_digits n = 1 + number_digits (n `div` 10)

-- ж) Проверить, является ли заданное число простым.

isPrime :: Int -> Bool
isPrime n
	| n <= 0	 	 			= error "число должно быть больше 0"
	| (n > 0) && (n < 4)	= True
	| otherwise 			= recStep (n-1)
	where
		isNotDivided a b 	= (a `mod` b) /= 0
		recStep p
			| p == 1		= True
			| otherwise	= (isNotDivided n p) && recStep (p-1) 
		

-- 8) Разное

{-
   а) Дан номер года (положительное целое число). Определить количество дней в этом году,
  учитывая, что обычный год насчитывает 365 дней, а високосный — 366 дней. Високосным
  считается год, делящийся на 4, за исключением тех годов, которые делятся на 100 и
  не делятся на 400 (например, годы 300, 1300 и 1900 не являются високосными,
  а 1200 и 2000 — являются).
-}

nDays :: Int -> Int
nDays year
	| isLeap		= 366
	| otherwise	= 365
  where
    isLeap = ((year `mod` 4) == 0) && (((year `mod` 100) /= 0) || ((year `mod` 400) == 0))
