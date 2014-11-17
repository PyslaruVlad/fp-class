{-# Language FlexibleInstances #-}
import Control.Monad
import Control.Applicative
import System.Environment
import System.Random

{-
  Модифицируйте имеющуюся реализацию задачи о канатоходце (лекция 9) следующим образом:
  1) реализуйте загрузку входных данных из файла следующего вида:
       R 2
       L 3
       R -1
       B
       L 1
     и вычисление соответствующего им результата (в решении может пригодиться 
     функция foldr (<=<) return — проверьте её тип для получения подсказки);
  2) замените монаду Maybe на Either String так, чтобы в случае падения канатоходца
     можно было получить информацию о его причинах (нарушение баланса и в какую
     сторону или банан на канате);
  3) реализуйте операцию landBoth, поддерживающую одновременное (атомарное) приземление
     птиц на оба конца шеста, и внесите соответствующие изменения в другие функции;
  5) реализуйте операцию unlandAll (одновременный вылет всех птиц с шеста) и внесите
     соответствующие изменения в другие функции;
  4) организуйте масштабное тестирование.
-}

type Birds = Int

type Pole = (Birds, Birds)

balance = 3

updatePole :: Pole -> Maybe Pole
updatePole p = if unbalanced p then Nothing else Just p
  where
    unbalanced (l, r) = abs (l - r) > balance

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right) = updatePole (left + n, right)

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right) = updatePole (left, right + n)

banana :: Pole -> Maybe Pole
banana = const Nothing

tests = all test [1..3]
  where
    test 1 = (return (0, 0) >>= landLeft 1 >>= landRight 4 
              >>= landLeft (-1) >>= landRight (-2)) == Nothing
    test 2 = (return (0, 0) >>= landRight 2 >>= landLeft 2 >>= landRight 2) == Just (2, 4)
    test 3 = (return (0, 0) >>= landLeft 1 >>= banana >>= landRight 1) == Nothing








-- Первое задание

data ActionType = LeftA Int | Banana | RightA Int deriving (Show)
    
action :: ActionType -> Pole -> Maybe Pole
action (LeftA n) = landLeft n
action (RightA n) = landRight n
action Banana = banana

walkTheRope :: Pole -> [ActionType] -> Maybe Pole
walkTheRope = foldM (flip action)

readActions :: String -> [ActionType]
readActions = map (parse . words) . lines
    where
        parse [t, q]
            | t == "R"  = RightA (read q)
            | t == "L"  = LeftA (read q)
        parse ["B"] = Banana
        parse _ = error "Format error!\n"
   
walkOnTheFile :: Pole -> String -> IO (Maybe Pole)
walkOnTheFile pole fname = readFile fname >>= return . walkTheRope pole . readActions

-- Ответ. Можно запустить с любым файлом имеющим вышеуказанный формат.
firstTaskAnswer :: String -> IO (Maybe Pole)
firstTaskAnswer = walkOnTheFile (0,0)

{- 
    Например, в случае когда "02-steps.txt" состоит из
        R 2
        L 3
        R -1
        B
        L 1
        
    Результатом будет:
    >firstTaskAnswer "02-steps.txt"
    Nothing
    
    Если изменить содержимое файла выше, убрав B, то получим:
    >firstTaskAnswer "02-steps.txt"
    Just (4,1)
-}









-- Обобщение задачи.
-- Иногда мне кажется, что я с этим перебарщиваю...

-- Вот в это обозначение я не закладывал глубокого смысла. Просто писать меньше.
type PureP m = Pole -> m Pole

-- Все самое необходимое, чтобы отличить одну монаду над Pole от другой.
class RopeWalkerMonad m where
    landPosit :: PureP m -- Результат в случае успешной посадки (не имеет значений куда).
    leftNeg :: PureP m -- Результат в случае ошибки после посадки слева. 
    rightNeg :: PureP m -- То же самое, но справа.
    absBanana :: PureP m -- Банан на канате.
    
-- Любые изменения типов событий затрагивают в первую очередь этот тип данных.
data EventType = LandLeft Birds | UnlandAll | BananaEvent | 
                 LandBoth Birds Birds | LandRight Birds

-- Проверка перевеса.
absCheckLanding :: RopeWalkerMonad m => PureP m
absCheckLanding p@(l, r)
    | l - r > absBalance    = leftNeg p
    | r - l > absBalance    = rightNeg p
    | otherwise             = landPosit p
    where
        absBalance = 3




-- Обработка различных типов событий.

absLandBoth :: RopeWalkerMonad m => Birds -> Birds -> PureP m
absLandBoth ln rn (lm, rm) = absCheckLanding (lm + ln, rm + rn)

absLandLeft :: RopeWalkerMonad m => Birds -> PureP m
absLandLeft n p = absLandBoth n 0 p

absLandRight :: RopeWalkerMonad m => Birds -> PureP m
absLandRight n p = absLandBoth 0 n p

absUnlandAll :: RopeWalkerMonad m => PureP m
absUnlandAll pole = landPosit (0, 0)




-- Абстрактная ходьба по канату.

absAction :: RopeWalkerMonad m => EventType -> PureP m
absAction (LandLeft n) = absLandLeft n
absAction UnlandAll = absUnlandAll
absAction BananaEvent = absBanana
absAction (LandBoth l r) = absLandBoth l r
absAction (LandRight n) = absLandRight n

-- По сути, то что RopeWalkerMonad является монадой проявляется только здесь.
-- но для экономии времени я решил не писать это всюду, ибо это не есть необходимость.
-- (идею подсмотрел в нижке М. Липовача)
walkTheRopeJack :: (RopeWalkerMonad m, Monad m) => Pole -> [EventType] -> m Pole
walkTheRopeJack = foldM (flip absAction)





-- Генерация случайной игры.

-- Создает случайное событие, в котором если и присутствуют птицы,
-- то в количестве не большем, чем maxVal.
randomizeEvent :: RandomGen g => Int -> g -> (EventType, g)
randomizeEvent maxVal gen = getEventByNum $ randomR (1,4::Int) gen
    where
        getEventByNum (eN, sG)
            | eN == 1   = (\ (n, g) -> (LandLeft n, g)) $  makeVal sG
            | eN == 2   = (UnlandAll, sG)
            | eN == 3   = (LandBoth first second, lastGen) 
            | eN == 4   = (\ (n, g) -> (LandRight n, g)) $ makeVal sG
            -- | eN == 5   = (BananaEvent , sG)
            where
                makeVal g = randomR (-maxVal, maxVal) g
                (first, thirdGen) = makeVal sG
                (second, lastGen) = makeVal thirdGen
-- Решил убрать банан из случайных событий, ибо он появляется к сожалению почти в каждой
-- прогулке. А это сразу приводит к падению канатоходца и невозможности нормально
-- протестить алгоритмы.
                
-- Создает бесконечный список состоящий из событий, которые могут случиться с
-- канатоходцем.
randomizeGame :: RandomGen g => Int -> g -> [EventType]
randomizeGame maxVal gen = nextVal : randomizeGame maxVal nextGen
    where
        (nextVal, nextGen) = randomizeEvent maxVal gen
        
-- Генерирует конечную цепь событий, вовклекая генератор случайных чисел.
-- Количество событий варьируется от 5 до 10.
randomizeWalk :: Int -> IO [EventType]
randomizeWalk maxVal = newStdGen >>= return . randomR (5, 10::Int)
    >>= \ (q, newGen) -> return $ take q $ randomizeGame maxVal newGen
    

-- Не нашел применения этим двум функциям: (но жалко их стирать :( )  
         
-- Запускает случайную прогулку канатоходца.
performRandomWalk :: (RopeWalkerMonad m, Monad m) => Int -> IO (m Pole)
performRandomWalk mV = randomizeWalk mV >>= return . walkTheRopeJack (0,0)

-- n раз повторяет случайную прогулку.
performDifferentWalks :: (RopeWalkerMonad m, Monad m) => Int -> Int -> IO [m Pole]
performDifferentWalks n = mapM performRandomWalk . replicate n






-- Пора бы уже и конкретными задачами заняться.

instance RopeWalkerMonad Maybe where
    landPosit = Just
    leftNeg = const Nothing
    rightNeg = const Nothing
    absBanana = banana

data Message = LeftFall | BananaFall | RightFall deriving Eq
    
instance RopeWalkerMonad (Either Message) where
    landPosit = Right
    leftNeg = const $ Left LeftFall
    rightNeg = const $ Left RightFall
    absBanana = const $ Left BananaFall 







-- Тесты.

writeAnswers :: [Bool] -> IO ()
writeAnswers conds = if and conds then putStrLn passed else putStrLn havent
    where
        passed = "All tests have been passed."
        havent = "At least one test have not been passed."


-- Тесты Maybe.

maybeTest :: Int -> Bool

maybeTest 1 = walkTheRopeJack (0,0) [LandLeft 1, LandBoth 2 3, LandRight (-3)]
    == Just (3, 0)
    
maybeTest 2 = walkTheRopeJack (0,0) [LandLeft 1, LandBoth 2 3, LandRight (-3)
    , LandLeft 1] == Nothing
    
maybeTest 3 = walkTheRopeJack (0,0) [LandLeft 1, LandBoth 2 3, LandRight (-3)
    , UnlandAll] == Just (0,0)
    
maybeTest 4 = walkTheRopeJack (0,0) [LandLeft 1, LandBoth 2 3, LandRight (-3)
    , UnlandAll, LandRight 3] == Just (0, 3)
    
maybeTest 5 = walkTheRopeJack (0,0) [LandLeft 1, LandBoth 2 3, LandRight (-3)
    , UnlandAll, LandRight 4] == Nothing
    
maybeTest 6 = walkTheRopeJack (0,0) [LandLeft 1, LandBoth 2 3, LandRight (-3)
    , UnlandAll, BananaEvent, LandRight 2] == Nothing

allMaybeTests :: IO ()
allMaybeTests = writeAnswers $ map maybeTest [1..6]

-- Вроде бы maybe работает правильно. 



-- Тесты (Either Message)
-- Поступил нагло и тупо скопировал тесты мэйби, чуть их переиначив.

eitherTest :: Int -> Bool

eitherTest 1 = walkTheRopeJack (0,0) [LandLeft 1, LandBoth 2 3, LandRight (-3)] 
    == (Right (3, 0) :: Either Message Pole)
    
eitherTest 2 = walkTheRopeJack (0,0) [LandLeft 1, LandBoth 2 3, LandRight (-3)
    , LandLeft 1] == Left LeftFall
    
eitherTest 3 = walkTheRopeJack (0,0) [LandLeft 1, LandBoth 2 3, LandRight (-3)
    , UnlandAll] == (Right (0,0) :: Either Message Pole)
    
eitherTest 4 = walkTheRopeJack (0,0) [LandLeft 1, LandBoth 2 3, LandRight (-3)
    , UnlandAll, LandRight 3] == (Right (0, 3) :: Either Message Pole)
    
eitherTest 5 = walkTheRopeJack (0,0) [LandLeft 1, LandBoth 2 3, LandRight (-3)
    , UnlandAll, LandRight 4] == Left RightFall
    
eitherTest 6 = walkTheRopeJack (0,0) [LandLeft 1, LandBoth 2 3, LandRight (-3)
    , UnlandAll, BananaEvent, LandRight 2, LandRight 3] == Left BananaFall

allEitherTests :: IO ()
allEitherTests = writeAnswers $ map eitherTest [1..6]

-- И (Either Message) на тестах сработал.





-- Условное сравнение результатов прогулок по канату.

compareMaybeEither :: Maybe Pole -> Either Message Pole -> Bool
compareMaybeEither Nothing (Left _) = True
compareMaybeEither (Just a) (Right b) = a == b
compareMaybeEither _ _ = False


-- Взаимная проверка с использованием случайных цепочек событий.
-- Например     maybeEitherTest 10000 3
-- проверяет на совпадения результатов (Either Message) и Maybe на 10000 различных
-- цепочках событий (3 - макисмимальное количество птиц, которые могут участвовать в
-- одном событи; или 6, если учитывать приземление на оба конца шеста).
-- Напомню, что я исключил конкретно из этого вида тестов банан, по причинам приведенным выше.

maybeEitherTest :: Int -> Int -> IO ()
maybeEitherTest d mV = iowalks >>= \ walks -> maybeResults walks >>= \ mRpure 
    -> eitherResults walks >>= \ eRpure -> 
    writeAnswers $ getZipList $ compareMaybeEither <$> (ZipList mRpure) <*> (ZipList eRpure)
    where
        iowalks = mapM randomizeWalk $ replicate d mV
        performWalks ws f = return $ map (f (0,0)) ws
        maybeResults ws = performWalks ws walkTheRopeJack :: IO [Maybe Pole]
        eitherResults ws = performWalks ws walkTheRopeJack :: IO [Either Message Pole]
        
