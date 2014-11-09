import System.Environment
import System.Random
import Data.Functor

{-
  Напишите функцию reduce, принимающую один целочисленный аргумент a и возвращающую 0,
  если аргумент делится на 3, a^2, если он на 3 не делится и является при этом нечётным,
  a^3 в остальных случаях.
-}

reduce :: Integral a => a -> a
reduce a
    | a `mod` 3 == 0    = 0
    | odd a             = a * a
    | otherwise         = a * a * a

{-
  Напишите функцию, применяющую функцию reduce заданное количество раз к значению в контексте,
  являющимся функтором:
-}

reduceNF :: (Functor f, Integral a) => Int -> f a -> f a
reduceNF n fa = foldr fmap fa $ replicate n reduce

{-
  Реализуйте следующие функции-преобразователи произвольным, но, желательно, осмысленным и
  нетривиальным способом.
-}

toList :: Integral a => [(a, a)]  -> [a]
toList = fmap (\ (a, b) -> gcd a b)

toMaybe :: Integral a => [(a, a)]  -> Maybe a
toMaybe [] = Nothing
toMaybe ls
    | finAcc == 0   = Nothing
    | otherwise     = Just finAcc
    where
        finAcc = foldl (\ acc (x,y) -> acc + min x y) 0 ls

toEither :: Integral a => [(a, a)]  -> Either String a
toEither [] = Left "Empty list"
toEither ls
    | finAcc == 0   = Left "Null result"
    | otherwise     = Right finAcc
    where
        finAcc = foldl (\ acc (x,y) -> acc + gcd x y) 0 ls

-- воспользуйтесь в этой функции случайными числами
toIO :: (Integral a, Random a) => [(a, a)]  -> IO a
toIO ls = do
    gen <- newStdGen
    let
        func (acc, g) (x, y) = (newVal + acc, newG)
            where
                (newVal,newG) = randomR (min x y, max x y) g
    return $ fst $ foldl func (1, gen) ls

{-
  В параметрах командной строки задано имя текстового файла, в каждой строке
  которого записана пара целых чисел, разделённых пробелами. Загрузите
  данные из файла в список пар целых чисел, преобразуйте этот список к
  значениям в контекстах [], Maybe, Either String и IO и примените к каждому
  из контекстов функцию reduceNF (значение N также должно браться из 
  параметров командной строки).
-}

parseArgs :: [String] -> (FilePath, Int)
parseArgs (fp:numStr:_) = (fp, read numStr)

readData :: FilePath -> IO [(Int, Int)]
readData fp = fmap (makePairs . lines) $ readFile fp
    where
        makePairs = foldl mkP []
        mkP acc str = flip (:) acc $ pair $ map read $ words str
        pair (a:b:_) = (a,b)

main = do
  (fname, n) <- parseArgs `fmap` getArgs
  ps <- readData fname
  putStr "List: "
  print $ reduceNF n (toList ps)
  putStr "Maybe: "
  print $ reduceNF n (toMaybe ps)
  putStr "Either: "
  print $ reduceNF n (toEither ps)
  putStr "IO: "
  reduceNF n (toIO ps) >>= print

{-
  Подготовьте несколько тестовых файлов, демонстрирующих особенности различных контекстов.
  Скопируйте сюда результаты вызова программы на этих файлах.
  
    *Main> :main 02-file1.txt 2
    List: [1,512,0,1,1]
    Maybe: Just 7890481
    Either: Right 14641
    IO: 88529281

    *Main> :main 02-file2.txt 1
    List: [1,8,1,0,1,1]
    Maybe: Just (-10648)
    Either: Right 0
    IO: 2515456

    *Main> :main 02-file3.txt 3
    List: [1,134217728,134217728]
    Maybe: Just 0
    Either: Right 390625
    IO: -6250996282790248448

-}

