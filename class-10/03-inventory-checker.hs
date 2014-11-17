import Control.Monad
import Data.List
import System.Environment
import qualified Data.Map.Lazy as M

{-
   Дан текстовый файл (inventory.txt)  с перечислением всей имеющейся на складе
   лёгкой брони. Сформируйте список имеющихся полных комплектов брони одного
   вида (kind). Указание: в решении рекомендуется пользоваться монадическими
   операциями всюду, где только возможно.
-}

data ArmorType = Shield | Helmet | Gauntlets | Boots | Cuirass
   deriving (Show, Read, Eq, Ord, Enum, Bounded)
data ArmorKind = Chitin | Hide | Leather | Elven | Scaled | Glass | ImperialLight
   deriving (Show, Read, Eq, Ord, Enum, Bounded)
data ArmorItem = ArmorItem ArmorKind ArmorType 
   deriving (Show, Eq)
data ArmorKit = ArmorKit ArmorKind [ArmorType]
   deriving (Show, Eq)

loadInventory :: FilePath -> IO [ArmorItem]
loadInventory fname = readFile fname >>= return . map (parseItem . words) . lines
    where
        parseItem [kindStr, typeStr] = ArmorItem (read kindStr) (read typeStr)

-- Считает сколько разных единиц брони кокнретных типов из types и вида kind
-- содержиться в списке items.
countTypes :: ArmorKind -> [ArmorType] -> [ArmorItem] -> [(ArmorType, Int)]
countTypes kind types items = M.toList updatedTypesSet
    where
        typesSet = foldl (\ ac tp -> M.insert tp 0 ac) M.empty types
        updatedTypesSet = foldl updateFunc typesSet items
        updateFunc mapAcc (ArmorItem k armType)
            | k == kind = M.update (return . (+1)) armType mapAcc
            | otherwise = mapAcc

-- Функция findMinKit определяет возможно ли скомплектовать один полный набор 
-- брони вида kind. 
buildArmorKit :: ArmorKind -> [ArmorItem] -> Maybe ArmorKit
buildArmorKit kind = findMinimalKit . unzip . typesCounts
    where
        typesCounts = countTypes kind [minBound::ArmorType .. maxBound]
        findMinimalKit (kit, vals)
            | minimum vals == 0 = Nothing
            | otherwise         = Just $ ArmorKit kind kit
             
buildKits :: [ArmorItem] -> Maybe [ArmorKit]
buildKits items = if justKits == [] then Nothing else sequence justKits
    where
        allKinds = [minBound::ArmorKind .. maxBound]
        justKits = filter (/=Nothing) $ map (flip buildArmorKit items) allKinds

main = (head `liftM` getArgs) >>= loadInventory  >>= return . buildKits >>= print

