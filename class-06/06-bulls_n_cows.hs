{-
   Запрограммируйте игру «Быки и коровы» (https://ru.wikipedia.org/wiki/Быки_и_коровы)
   в варианте «компьютер загадывает — пользователь отгадывает».
-}

import System.Environment
import System.Random
import Data.List

main = do
    gen <- newStdGen
    let
        secret = generateSomeRandom gen 4 ['0'..'9']
    putStr "Я загадал слово! Попробуй отгадать.\n"
    gameCycle secret
    



gameCycle :: [Char] -> IO ()
gameCycle secret = do
    putStr "Каково будет ваше предположение?\n"
    nextL <- getLine
    let
        attempt = nub nextL
    if length nextL /= length secret
        then (do
            putStr "Длина догадки должна совпадать c длиной загадки!\n"
            gameCycle secret)
    else if length attempt /= length secret
        then (do
            putStr "Ну зачем же повторять символы! Я так не хочу играть!\n"
            putStr "Но я сегодня добрый и дам тебе шанс исправиться.\n"
            gameCycle secret)
        else do
            let
                check = checkWithSecret secret attempt
            if check == (0, length secret)
                then (do
                    putStr "Вы отгадали! Не может быть!!!\n"
                    putStr "(нажмите enter для выхода)\n"
                    s <- getLine
                    return ())
                else (do
                    putStr "Нет. Вам не повезло :( Попробуйте еще раз.\n"
                    putStr $ "Даю подсказку: число коров - "++(show $ fst check)
                    putStr $ "; число быков - "++(show $ snd check)
                    putStr "\n"
                    gameCycle secret)
            



-- Область чистых функций


type Cows = Int
type Bulls = Int

generateSomeRandom :: (RandomGen g, Eq a) => g -> Int -> [a] -> [a]
generateSomeRandom = getNext
    where
        getNext _ 0 _   = []
        getNext g d dom = nextVal: (getNext nextGen (d-1) $ filter (/=nextVal) dom)
            where
                nextVal =  dom!! (fst $ randomR (1, length dom-1) g)
                nextGen = snd $ randomR (1, length dom-1) g


checkWithSecret :: Eq a => [a] -> [a] -> (Cows, Bulls)
checkWithSecret sec att = (cows, bulls)
    where
        succIfElem c e = if e `elem` sec then c+1 else c
        countFunc (b, c, s:ss) t
            | t == s    = (b+1, c, ss)
            | otherwise = (b, succIfElem c t, ss)
        (bulls, cows, _) = foldl countFunc (0, 0, sec) att
    

