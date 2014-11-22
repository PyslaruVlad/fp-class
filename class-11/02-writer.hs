import Control.Monad.Writer
import Control.Monad
import Control.Applicative

data CalcState = CState { getSum::Double,  getMult::Double,  index::Int}
type SeriesFunction = CalcState -> CalcState
type SeriesCalulation = Writer [Double] CalcState

performCalc :: SeriesFunction -> SeriesCalulation -> SeriesCalulation
performCalc func w = w >>= \ stp@(CState s m i) -> tell [m] >> return (func stp)

calcTaylorSeries :: Double -> Double -> SeriesFunction -> (Double, [Double])
calcTaylorSeries s eps func = makeAns $ runWriter $ performWhile 0 $ writer (CState 0 s 1, [])
    where
        makeAns ((CState s m i), jrnl) = (s, jrnl)
        performWhile prev w = performCalc func w >>= \ stp@(CState next m i) -> 
            if abs(prev-next) < eps then return stp else performWhile next (return stp)

calcSin :: Double -> Double -> (Double, [Double])
calcSin eps x = calcTaylorSeries x eps (\ (CState s m i) 
    -> CState (nextSum s m) (nextMult m i) (i+1))
    where
        nextSum a p = a + p
        nextMult a n = - x * x * a / (2 * fromIntegral n) / (1 + 2 * fromIntegral n)
        
calcCos :: Double -> Double -> (Double, [Double])
calcCos eps x = calcTaylorSeries 1 eps (\ (CState s m i)
    -> CState (nextSum s m) (nextMult m i) (i+1))
    where
        nextSum a p = a + p
        nextMult a n = - x * x * a / (2 * fromIntegral n - 1) / (2 * fromIntegral n)

checkSinCos = compare chSins ansSins && compare chCoss ansCoss
    where
        args = (\ n -> 2 * pi / 10000 * fromIntegral n) <$> ZipList [1..10000]
        eps = 0.000000000001
        chSins = (fst . calcSin eps) <$> args
        chCoss = (fst . calcCos eps) <$> args
        ansSins = sin <$> args
        ansCoss = cos <$> args
        compare l1 l2 = and $ getZipList $ (\ a b -> abs (a-b) < eps) <$> l1 <*> l2
        
