{-# LANGUAGE EmptyDataDecls #-}

module GrahamScan where

-- 1. Определить тип Point для хранения информации о точке на вещественной плоскости.

data Point = Point
    {
        x :: Double,
        y :: Double
    }
    deriving (Eq)

instance Show Point where
    show (Point x y) = "{"++(show x)++","++(show y)++"}\n"

{-
  2. Если заданы три точки a, b, c, можно рассматривать направление поворота от отрезка прямой,
  заключённого между точками a и b, к отрезку прямой, заключённому между точками b и c. Поворот
  может осуществляться влево, вправо или отрезки могут лежать на одной прямой — для представления
  этих трёх возможностей определить специальный тип Direction.
-}

data Direction = RightRotation | SameDirection | LeftRotation
    deriving (Show, Eq, Ord)

{-
  3. Определить функцию, которая принимает список точек и вычисляет список направлений поворотов
  для каждых трёх последовательных точек. Например, для списка точек [a, b, c, d, e] она возвращает
  список поворотов для точек [a, b, c], [b, c, d] и [c, d, e]. При написании этой функции рекомендуется
  определить несколько вспомогательных функций.
-}

data Vector = Vector Point Point

vecProdMod :: Vector -> Vector -> Double
vecProdMod (Vector (Point ax ay) (Point axe aye))
           (Vector (Point bx by) (Point bxe bye))
                = (axe-ax)*(bye-by) - (aye-ay)*(bxe-bx)
vecModule :: Vector -> Double
vecModule (Vector (Point x1 y1) (Point x2 y2)) = sqrt $ (x2-x1)^2 + (y2-y1)^2

calcForVectors :: Vector -> Vector -> Direction
calcForVectors v1 v2
    | vecProdMod v1 v2 > 0  = LeftRotation
    | vecProdMod v1 v2 < 0  = RightRotation
    | otherwise             = SameDirection
        

calcForPoints :: [Point] -> Direction
calcForPoints [a, b, c] = calcForVectors (Vector b a) (Vector b c)
calcForPoints _ = error "эту функцию можно посчитать только для трех точек"

divideInGroups :: [a]-> [[a]]
divideInGroups xs 
    | length xs < 3 = []
    | otherwise     = (take 3 xs): (divideInGroups $ drop 1 xs)


directions :: [Point] -> [Direction]
directions = (map calcForPoints) . divideInGroups

directions_test1 = directions [(Point 0.243 24.0), (Point (-1.0) 1.0), (Point 54.0 (-12.54))
                              ,(Point (-1.0) 1.0)] 
                              == [RightRotation, SameDirection]
                              
directions_test2 = directions [(Point 0.9 60.923), (Point (-12.39) 5), (Point 10.6 (-24.4))
                              ,(Point 15.35 0.0001), (Point 5.13 (-100))] 
                              == [RightRotation, RightRotation, LeftRotation]                          

{-
  4. Пользуясь решениями предыдущих упражнений, реализовать алгоритм Грэхема нахождения выпуклой
  оболочки множества точек на вещественной плоскости. Описание алгоритма можно взять в английском
  (Graham scan) или русском разделах Википедии. Там же можно разобраться с тем, что именно называют
  выпуклой оболочкой (convex hull). Визуализация порядка работы алгоритма имеется на Youtube:
  http://www.youtube.com/watch?v=BTgjXwhoMuI
-}


pointsList :: Int -> [Point]
pointsList 1 =  [Point 2 2, Point 1 1, Point 4 3, Point 1 2, Point 2 5]
pointsList 2 =  [Point 0 0, Point (-3) (-3), Point 1.1 (-4), Point  0.5 1
                , Point (-2.5) 4, Point 0 1, Point 2 2]
pointsList 3 =  [Point (-4) 6, Point 0 6 , Point (-1.9) (-1.002)
                , Point (-3.5) (-0.05), Point 3 (-1.7), Point 0 1
                , Point (-3) 4, Point (-1) 1, Point 2 1 , Point 3 4]
pointsList _ = error "больше наборов точек я не придумал"

angAndMod p zeroP = (negate $ asin $ vPM/pM, vecModule (Vector zeroP p))
    where
        pM  = (vecModule $ zeroVec zeroP) * (vecModule (Vector zeroP p))
        vPM = vecProdMod (zeroVec zeroP) (Vector zeroP p)
        zeroVec zP = Vector zP (Point (x zP+1) (y zP))

sortPoints :: Point -> Vector -> [Point] -> [Point]
sortPoints zeroP zeroVec = takePoints . foldr insertPoint [] 
    where
        angAndMod p = (asin $ vPM/pM, vecModule (Vector zeroP p))
            where
                pM  = (vecModule zeroVec) * (vecModule (Vector zeroP p))
                vPM = vecProdMod zeroVec (Vector zeroP p)
        insertPoint p [] = [(fst $ angAndMod p, snd $ angAndMod p, p)]
        insertPoint p (curP@(a,m,_):acc)
            | angP > a  = curP:insertPoint p acc
            | angP < a  = (angP, modP, p):curP:acc
            | modP > m  = curP:insertPoint p acc
            | otherwise = (angP, modP, p):curP:acc
            where
                (angP, modP) = angAndMod p
        takePoints = foldr (\ (_,_,p) acc -> p:acc) []


sortedList :: [Point] -> [Point]
sortedList list = bp : sortPoints bp (zeroVec bp) newList
    where
        botomPoint = foldr1 (\ p acc -> if (y p) < (y acc) then p else acc)
        (bp, newList) = (botomPoint list, filter (/= bp) list)
        zeroVec zeroP = Vector zeroP (Point ((x zeroP)) (y zeroP+1))
        

graham_scan :: [Point] -> [Point]
graham_scan list = reverse $ getRight ([l1,bp], ls)
    where
        newList@(bp:l1:ls) = (sortedList list)
        nRot (c:b:a:ps) = calcForPoints [a,b,c]
        getRight (la, (pc:psl)) 
            | psl  == []                    = pc:la
            | nRot (pc:la) == LeftRotation  = getRight (procDel pc la, psl)
            | otherwise                     = getRight (pc:la, psl)
        procDel pc [pb,pa] = [pc,pb,pa]
        procDel pc a@(pb:pa:lst)
            | nRot (pc:a) == LeftRotation = procDel pc (pa:lst)
            | otherwise = (pc:a)
    

{-
  5. Приведите несколько примеров работы функции graham_scan.
-}

                
answer :: Int -> [Point]
answer 1 = [Point 1 1, Point 4 3, Point 2 5, Point 1 2]
answer 2 = [Point 1.1 (-4), Point 2 2, Point (-2.5) 4, Point (-3) (-3)]
answer 3 = [Point 3 (-1.7), Point 3 4, Point 0 6, Point (-4) 6
           , Point (-3.5) (-0.05), Point (-1.9) (-1.002)]
answer _ = error "я не придумал еще ответ на несуществующий вопрос"


-- Вызываейте любую от 1 до 3 
grahamScanTest i = (answer i ==) $  graham_scan $ pointsList i


