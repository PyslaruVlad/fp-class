{-
  Напишите программу, решающую следующую задачу методом полного перебора:

     «Крестьянину нужно перевезти через реку волка, козу и капусту. Но лодка такова,
     что в ней может поместиться только крестьянин,  а с ним или один волк, или одна
     коза, или одна капуста.  Но если оставить волка с козой,  то волк съест козу, а
     если оставить  козу с капустой,  то коза  съест капусту.  Как перевёз свой груз
     крестьянин?»

  В качестве идеи для реализации используйте решение задачи о калотанской семье
  (kalotans-puzzle.hs). 
-}

import Data.List
import Control.Monad
import Control.Monad.State

data Item = Cabbage | Goat | Wolf
    deriving (Eq, Show)

data Action = To Item | From Item | FromEmpty -- From destination - To destination
    deriving (Eq, Show)
-- решил, что только в одну сторону возможны пустые перемещения. ибо мотаться туда
-- сюда в холостую - бессмысленно.

type TrLog = [Action]
data TrState = TrState { darkSide::[Item], lightSide::[Item], tLog::TrLog, breakP::Int }
-- Движение от "темного" к "светлому". break - соовтетствует точке останова. ниже будет
-- ясно, что break должен быть больше чем минимально необходимое число шагов за которые
-- фермер сможет перетащить весь свой груз.

checkForCrash :: [Item] -> Bool
checkForCrash items = not goatEatsCab && not wolfEatsGoat
    where
        goatEatsCab = Cabbage `elem` items && Goat `elem` items
        wolfEatsGoat = Goat `elem` items && Wolf `elem` items

perfTrToWith :: Item -> StateT TrState [] ()
perfTrToWith item = do
    st <- get
    let 
        nD = delete item $ darkSide st
        nL = (:) item $ lightSide st
        nLg = (To item) : tLog st
        newState = st { darkSide = nD, lightSide = nL, tLog = nLg }
    put newState
    guard $ checkForCrash nD

perfTrFromWith :: Item -> StateT TrState [] ()
perfTrFromWith item = do
    st <- get
    let 
        nD = (:) item $ darkSide st
        nL = delete item $ lightSide st
        nLg = (From item) : tLog st
        newState = st { darkSide = nD, lightSide = nL, tLog = nLg }
    put newState
    guard $ checkForCrash nL

perfTrFromEmpty :: StateT TrState [] ()
perfTrFromEmpty = do
    st <- get
    let 
        newLog = FromEmpty : tLog st
        light = lightSide st
    put $ st { tLog = newLog }
    guard $ checkForCrash light

transpGarbage :: StateT TrState [] ()
transpGarbage = do
    dark <- gets darkSide
    let perfToWithAll = msum $ map perfTrToWith dark
    perfToWithAll
    dark <- gets darkSide
    if dark == []
    then return ()
    else do
        light <- gets lightSide
        let perfFromWithAll = msum $ map perfTrFromWith light
        perfFromWithAll `mplus` perfTrFromEmpty
        checkBreakpoint
        transpGarbage


-- пожалуй самая важная функция всей это программы. пока еще нe придумал, как иначе 
-- заставить алгоритм избежать зацикливаний.
checkBreakpoint :: StateT TrState [] ()
checkBreakpoint = do
    lg <- gets tLog
    maxLength <- gets breakP
    guard $ length lg <= maxLength
        

-- Проверочка.

-- минимально необходимое число ходов - 7. Поэтому 10 подходит в качестве точки останова.

main = do
    let 
        takeMin = reverse . head . sortBy (\ xs ys -> compare (length xs) (length ys))
        initState = TrState [Cabbage, Goat, Wolf] [] [] 10
        actions = takeMin $ map tLog $ execStateT transpGarbage initState
    print actions

