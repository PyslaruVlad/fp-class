import Control.Monad
{- Пользуясь списком как монадой, вычислите пересечение  заданных списков -}
intersect :: Eq a => [[a]] -> [a]
intersect = foldr1 (\ ls acc -> concat $ placeInBra `liftM` acc `ap` ls) 
    where
        placeInBra e1 e2 = if e1 == e2 then [e1] else []


chek_intersect = all (== ans) $ intersect `liftM` [case1, case2, case3]
    where
        case1 = [[1..10], [1..5], ans]
        case2 = [[0..4],[-10..100],[-10..4],[2..100]]
        case3 = [[1..5], ans, [0..6]]
        ans = [2,3,4]
