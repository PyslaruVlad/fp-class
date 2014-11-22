import Control.Monad.Writer
import Control.Monad
import Control.Applicative
import Data.List

-- Возможно все слишком усложнил опять.

type Summand = Double
type Accum = Double
data CalcState = CState Accum Summand Int
type CalcFunction = CalcState -> CalcState
type SeriesCalulation = Writer [Double] CalcState

performCalc :: CalcFunction -> SeriesCalulation -> SeriesCalulation
performCalc func w = w >>= \ stp@(CState _ m _) -> tell [m] >> return (func stp)

calcTaylorSeries :: Double -> Double -> CalcFunction -> (Double, [Double])
calcTaylorSeries s eps func = makeAns $ runWriter $ performWhile 0 $ writer (CState 0 s 1, [])
    where
        makeAns ((CState s _ _), jrnl) = (s, jrnl)
        performWhile prev w = performCalc func w >>= \ stp@(CState next _ _) -> 
            if abs(prev-next) < eps then return stp else performWhile next (return stp)


-- Ответы.

calcSimpleTaylor :: Double -> Double -> (Summand -> Int -> Summand) -> (Double, [Double])
calcSimpleTaylor eps s nextMult = calcTaylorSeries s eps (\ (CState s m i) 
    -> CState (s+m) (nextMult m i) (i+1))

calcSin :: Double -> Double -> (Double, [Double])
calcSin eps x = calcSimpleTaylor eps x (\ a n 
    -> - x * x * a / (2 * fromIntegral n) / (1 + 2 * fromIntegral n))
        
calcCos :: Double -> Double -> (Double, [Double])
calcCos eps x = calcSimpleTaylor eps 1 (\ a n 
    -> - x * x * a / (2 * fromIntegral n - 1) / (2 * fromIntegral n))


-- Проверки.

args :: [Double]
args = (\ n -> 2 * pi / 10000 * fromIntegral n) `liftM` [1..10000]

eps :: Double
eps = 0.000000000001

compareZip :: ZipList Double -> ZipList Double -> Bool
compareZip l1 l2 = and $ getZipList $ (\ a b -> abs (a-b) < eps) <$> l1 <*> l2

-- Проверка совпадений значений (с заданной точностью) c заведомо правильными функциями
-- синуса и косинуса.
checkSinCos_onlyAnswers :: Bool
checkSinCos_onlyAnswers = compareZip chSins ansSins && compareZip chCoss ansCoss
    where
        makeList func = func <$> ZipList args
        chSins = makeList (fst . calcSin eps)
        chCoss = makeList (fst . calcCos eps)
        ansSins = makeList sin
        ansCoss = makeList cos

-- Проверка равенста сумм от списков справа и значений слева.
checkSinCos_sums :: Bool
checkSinCos_sums = compareVals calcCos && compareVals calcSin
    where
        compareVals func = compareUnziped $ unzip $ (findSum . func eps) `liftM` args
        findSum (val, ls) = (val, sum ls)
        compareUnziped (ls1, ls2) = compareZip (ZipList ls1) (ZipList ls2)



