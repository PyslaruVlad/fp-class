{-# LANGUAGE EmptyDataDecls #-}

module Drunkard where

{-
  1. Определить типы данных, необходимые для представления игральной карты в игре «Пьяница»,
  учитывая, что всего в колоде 52 карты.
-}

data Suit = Spades | Clubs | Diamonds | Hearts 
    deriving (Show, Eq, Enum, Bounded)

data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | 
             Ten | Jack | Queen | King | Ace 
    deriving (Show, Eq, Ord, Enum, Bounded)

data Card = Card
    { value ::Value
    , suit :: Suit
    }
    deriving (Eq)

instance Show Card where
    show (Card val s) = (show s)++"-"++(show val)


-- Это я сделал для удобства создания карт.
type ValueNum = Int
type SuitNum = Int
type TwoDecks = ([Card],[Card])


card :: SuitNum -> ValueNum -> Card
card s v
    | (s<1 || s>4) || (v<2 || v>14) = error "Parameters are out of borders." 
    |otherwise  = Card (toEnum $ v-2) (toEnum $ s-1)

spades, clubs, diamonds, hearts :: ValueNum -> Card
spades = card 1
clubs = card 2
diamonds = card 3
hearts = card 4

-- 2. Определить функцию, проверяющую, что две переданные ей карты одной масти.

sameSuit :: Card -> Card -> Bool
sameSuit c1 c2 = suit c1 == suit c2

{-
  3. Определить функцию, проверяющую, что переданная ей первой карта старше второй
  (масть в игре «Пьяница» игнорируется). Возвращённое значение EQ означает, что обе
  карты одинакового старшинства.
-}

beats :: Card -> Card -> Ordering
Card Two _ `beats` Card Ace _ = GT
Card Ace _ `beats` Card Two _ = LT
c1 `beats` c2 = value c1 `compare` value c2

{-
  4. Определить функцию, которая по паре списков карт возвращает новую пару списков карт
  с учетом правил игры «Пьяница» (один раунд игры): 
    * из вершин списков берутся две карты и добавляются в конец того списка, карта из
      которого старше оставшейся;
    * если первые взятые карты совпадают по достоинству, то из списков берутся и
      сравниваются следующие две карты (и так до тех пор, пока не будет определён победитель
      раунда).
-}

{-
    Правила гласят, что в колоду сначала кладется победившая карта, а под нее побежденная.
    Поэтому я тут сделал так сложно. Так же, согласно правилам, если карты закончились у
    одного игрока, то он берет случайную из колоды противника. Однако ввиду того, что
    скорее всего здесь не предполагается такой сложности я решил, что на этом игра 
    должна закончится - выигрывает тот, у кого остались карты.
-}

game_round :: TwoDecks -> TwoDecks -- я позволил себе изменить аннотацию типа.
game_round cardsPair = performStep [] cardsPair
    where
        revPairs [] = [] -- эта функция как раз и нужна, чтобы расположить элементы
        revPairs (x:y:xs) = y:x:revPairs xs -- в определенном порядке
        performStep acc ([], s2) = ([], s2) -- не добавляю аккумулятор ходов. не вижу
        performStep acc (s1, []) = (s1, []) -- смысла, ибо игра и так заканчивается.
        performStep acc ((c1:c1s), (c2:c2s))
            | c1 `beats` c2 == GT = (c1s++(c1:c2:reverse acc),c2s)
            | c1 `beats` c2 == LT = (c1s,c2s++c2:c1:(revPairs.reverse) acc)
            | otherwise = performStep (c2:c1:acc) (c1s, c2s)

-- Случай ничьей. Колоды "равноценны".
game_round_test1 =  secondIsEmpty && firstIsEmpty
    where
        secondIsEmpty = ([]==) $ snd $ round
        firstIsEmpty  = ([]==) $ fst $ round
        round = game_round (
            [diamonds 2, clubs 3, hearts 4],
            [spades 2, diamonds 3, clubs 4])
    
-- Вот в таком случае первая колода всего лишь на одну незначительную карту больше
-- второй колоды. И поэтому выигрвает первый игрок.
game_round_test2 = firstIsNotEmpty && secondIsEmpty
    where
        secondIsEmpty   = ([]==) $ snd $ round
        firstIsNotEmpty = ([]/=) $ fst $ round
        round = game_round (
            [diamonds 2, clubs 3, hearts 4, hearts 2],
            [spades 2, diamonds 3, clubs 4])

{-
  5. Определить функцию, которая по паре списков возвращает количество раундов, необходимых
  для завершения игры (одна из колод оказывается пустой), и номер победителя.
-}

-- И все-таки ничья тоже возможна, если быть совсем честным.
data Winner = First | Second | Draw
    deriving (Show, Eq)

game :: TwoDecks -> (Winner, Int)
game = round 0
    where
        round c ([],[]) = (Draw, c)
        round c ([],_)  = (Second, c)
        round c (_,[])  = (First, c)
        round c cPair   = round (c+1) $ game_round cPair
                             -- Игра записывает именно количество раундов,
                             -- но не количество ходов.

{-
  6. Приведите здесь результаты как минимум пяти запусков функции game (в каждом списке
  изначально должно быть не менее 10 карт).
-}

cardsPck :: Int -> TwoDecks

-- Как видно в этой раскладке первая колода строго меньше по значениям чем вторая
cardsPck 1 = (
      [spades 2, diamonds 3, clubs 4, hearts 5, diamonds 6
      , clubs 7, hearts 8, spades 9, spades 10, clubs 11],
      [spades 3, diamonds 4, clubs 5, hearts 6, diamonds 7
      , clubs 8, hearts 9, clubs 10, clubs 11, hearts 12]
      )
-- Первая половина первой колоды строго больше первой половины второй колоды.
-- C вторыми половинамив - ровно на оборот. это позволяет сначала первую половину
-- второй колоды "перебросить" в первую колоду, а затем самые большие карты
-- первой колоды "перебросить" во вторую. таким образом за 10 ходов вторая 
-- колодо становится "мажорирующей" над первой. и остается 10 ходов до ее победы.
cardsPck 2 = (
      [spades 3, diamonds 4, clubs 5, hearts 6, diamonds 7
      , clubs 8, hearts 9, clubs 10, clubs 11, hearts 12],
      [spades 2, diamonds 3, clubs 4, hearts 5, diamonds 6
      , clubs 9, hearts 10, spades 11, spades 12, clubs 13]
      )
-- Здесь стопки карт чередуются по упорядочененности - в каждой последующей
-- паре карт отношение упорядоченности между картами меняется на противоположное
-- по сравнению с предыдущей парой. Значимость карт в паре растет.
-- Получается ситуация аналогичная предыдущей задаче.
cardsPck 3 = (
      [spades 3, diamonds 3, clubs 5, hearts 5, diamonds 7
      , clubs 7, hearts 9, clubs 9, clubs 12, hearts 12],
      [spades 2, diamonds 4, clubs 4, hearts 6, diamonds 6
      , clubs 8, hearts 8, spades 10, spades 11, clubs 13]
      )
-- Немного измененный предыдущий вариант, в котором некоторые из пар карт
-- состоят из равнозначных элементов.
cardsPck 4 = (
      [spades 3, diamonds 3, clubs 5, hearts 5, diamonds 7
      , clubs 7, hearts 9, clubs 9, clubs 11, hearts 12],
      [spades 2, diamonds 4, clubs 5, hearts 6, diamonds 6
      , clubs 8, hearts 8, spades 10, spades 11, clubs 13]
      )
-- Абсолютно равноценные колоды.
cardsPck 5 = (
      [spades 3, diamonds 4, clubs 5, hearts 6, diamonds 7
      , clubs 8, hearts 9, clubs 10, clubs 11, hearts 12],
      [diamonds 3, clubs 4, hearts 5, diamonds 6, clubs 7
      , clubs 8, hearts 9, spades 10, spades 11, clubs 12]
      )
-- Почти равноценные колоды. Вторая на одну карту больше
cardsPck 6 = (
      [spades 3, diamonds 4, clubs 5, hearts 6, diamonds 7
      , clubs 8, hearts 9, clubs 10, clubs 11, hearts 12],
      [diamonds 3, clubs 4, hearts 5, diamonds 6, clubs 7
      , clubs 8, hearts 9, spades 10, spades 11, clubs 12, hearts 2]
      )
-- в этих раскладках я решил не создававть никакой упорядоченности
cardsPck 7 = (
      [spades 4, spades 2, diamonds 2, clubs 14, hearts 7
      , clubs 8, spades 13, spades 6, diamonds 10, clubs 4],
      [spades 12, spades 3, clubs 8, diamonds 7, hearts 2
      , clubs 10, hearts 10, clubs 5, spades 12, diamonds 5]
      )
cardsPck 8 = (
      [clubs 8, diamonds 4, spades 14, spades 2, hearts 3, clubs 13
      , diamonds 5, hearts 7, clubs 11, spades 10, clubs 9, diamonds 5],
      [spades 12, hearts 9, clubs 8, diamonds 8, spades 3, diamonds 13
      , clubs 13, hearts 8, clubs 10, clubs 2, diamonds 6, spades 5]
      )
cardsPck _ = error "игры под таким номером я не придумывал!"

gameNumber i = printGameRes $ game $ cardsPck i
    where
        printGameRes (w, c)
            | w == Draw = "Game ended in tie after "++(show c)++" rounds."
            | otherwise = (show w)++" player won the game after "++(show c)++" rounds."


{-
  7 (необязательное упражнение). Реализуйте версию функции game, которая помимо результатов
  игры возвращает запись всех ходов (карты, выкладываемые по ходу игры для сравнения).
-}

type Rounds = [(Card,Card)]
data GameResult = Result
    { win :: Winner
    , count :: Int
    , rnds :: Rounds
    }
    --deriving (Show)
-- Не хочу чтобы здесь был Show. Это просто ужасно.
-- И писать свой вариант тоже не хочу.


gameFrRnd :: TwoDecks -> (Rounds, TwoDecks)
gameFrRnd cardsPair = performStep ([],[]) cardsPair
    where
        revPairs [] = []
        revPairs (x:y:xs) = y:x:revPairs xs
        mkPr stps dcks = (stps, dcks)
        performStep (stps, acc) ([], s2) = mkPr stps ([], s2)
        performStep (stps, acc) (s1, []) = mkPr stps (s1, [])
        performStep (stps, acc) ((c1:c1s), (c2:c2s))
            | c1 `beats` c2 == GT = mkPr newStps (c1s++c1:c2:reverse acc, c2s)
            | c1 `beats` c2 == LT = mkPr newStps (c1s,c2s++c2:c1:(reverse . revPairs) acc)
            | otherwise = performStep (newStps, c2:c1:acc) (c1s, c2s)
            where
                newStps = (c1, c2):stps


gameFramed :: TwoDecks -> GameResult
gameFramed = round (0, [])
    where
        round (c, rnds) ([],[]) = Result Draw c $ reverse rnds
        round (c, rnds) ([],_)  = Result Second c $ reverse rnds
        round (c, rnds) (_,[])  = Result First c $ reverse rnds
        round (c, rnds) pair@((c1:c1s), (c2:c2s))
            = round ((c+1), fst nextRnd ++ rnds) $ snd nextRnd
            where
                nextRnd = gameFrRnd pair
            
-- Во всем остальном функция gameFramed работает также как и game, поэтому имеет смысл
-- выводить на экран только раунды. И только ту часть сравниваемых карт, которая соответствует
-- их значениям.
gameRoundsValues i = showRoundsValues $ gameFramed $ cardsPck i
    where
        showRoundsValues = (map trnsf) . rnds
        trnsf (c1, c2) = (value c1, value c2)


{-
  8 (необязательное упражнение). При выполнении функций из упражнений 4 и 5 возможно
  зацикливание. Чтобы его избежать, можно предусмотреть максимальное количество повторений
  (для раундов и ходов в рамках одного раунда). Подумайте, как обнаружить факт зацикливания
  в функции 4? Можно ли применить такой же подход в функции 5? Что нужно возвращать в случае
  обнаружения факта зацикливания? Измените соответствующим образом типовые аннотации и
  напишите безопасные по отношению к зацикливанию версии функций game_round и game.
-}


{-
    Мысли в слух:
    
    Только в том, случае когда функция вызывается рекурсивно возможно зацикливание. 
    Мой вариант функции game_round закачнивается всегда. Это легко увидеть, если посмотреть 
    на то место где функция хода в рамках одного раунда вызывается рекурсивно:
    performStep ... ((c1:c1s), (c2:c2s)) ... = performStep ... (c1s, c2s)
    Длина колод кард в этом случае всегда уменьшается на 1. При этом существуют шаблоны для
    тех случаев вызова функции, когда ее аргументами являются две колоды одна из которых пуста.
    И они - самое главное - не приводят к дальнейшим рекурсивным вызовам. Если мы конечно, не
    имеем дело с бесконечными колодами карт и правилами не указана необходимость такого вызова:
    перед функцией game_round я написал комментарий в котором указал, что в случае когда одна из
    колод оказывается пустой - правила предусматривают продолжение игры. Тогда тот игрок, что
    остался без карт должен вытягивать случайную карту из колоды противника. И даже в этом случае
    игра вряд ли зациклится, если количество карт конечно. Поэтому я и делаю вышеописанный вывод.
    
    То есть, обобщая, скажем, что один раунд, каким бы он ни был, закончится всегда. Но повторения
    раундов (и в частоности повторения цепочек ходов) возможны. Вот сюда и должна быть "направлена
    мысль". Нужно отслеживать цепочки ходов и искать среди них повторяющиеся. Однако, что либо
    определенное по поводу количества допустимых повторений цепочек, которые необходимо отслеживать 
    я не могу сказать. Пусть это будет внешний параметр.
    
    Поскольку необходимо следить за цепочками ходов, я предпочту использовать функции из раздела 7.
    Причем модернизировать можно как и саму функцию gameFrRnd - передавая ей в качестве параметра
    историю ходов, которую она будет анализировать и сравнивать с текущей завершенной цепочкой
    ходов, либо можно изменить функцию gameFramed - ту ее часть в которой в которой совершаются
    раунды. Однако в первом случае вероятность зацикливания частично сохраняется - если  учесть 
    возможность того, что цикл проявляется не в повторяющихся двух раундах, а в повторах серий 
    раундов.
    
    Сначала приведу пример зацикливающихся колод карт:
-}


cyclePck :: Int -> TwoDecks
cyclePck 1 = (
      [spades 2, diamonds 3, spades 4],
      [spades 3, diamonds 4]
      )
cyclePck _ = error "Такого зацикливающего случая я не придумывал!"

-- Я не дождался завершения этой функции.
doesntWork i = game $ cyclePck i



-- Тип данных возвращаемых игрой в зависимости от успешности исхода будет такой:
-- во втором случае вместе с результатом возвращаются плохие раунды.
data SafeGameResult = GameFinished Winner Int | GameAborted Rounds
    deriving (Eq)
 
instance Show SafeGameResult where
    show (GameFinished w c)
        | w == Draw = "Game ended in tie after "++(show c)++" rounds." 
        | otherwise =(show w)++" player won the game after "++(show c)++" rounds."
    show (GameAborted rnds)  = "Game was aborted in case of cycling: "++(show rnds)


-- Теперь напишу функцию, которая анализирует встречалась ли определенная цепочка ходов
-- (в рамках одного раунда) в игре, определенное количество раз.

containsStep :: (Eq a) => Int -> [a] -> a -> Bool
containsStep m log step = (m <=) $ foldr foldFunc 0 log
    where
        foldFunc x acc
            | x == step = acc+1
            | otherwise = acc

{- 
    Теперь можно сразу же написать функцию, которая анализирует каждую отдельную цепочку ходов
    в рамках одного раунда на встречаемость n-ое количество раз. Если последовательность встре-
    чается больше, либо n раз, то игра завершается с сообщением о зацикливании.
    
    Как я уже отмечал, сделать это возможно двумя способоми - модернезировав функцию game 
    (в моем случае gameFramed), что сделать куда как проще и нагляднее чем реализовать второй
    способ: game_round безопасную по отношению к повторяемости.
    
    Вот первый вариант:
-}

safeGame_inRounds :: Int -> TwoDecks -> SafeGameResult
safeGame_inRounds m = round (0, [])
    where
        round (c, rnds) ([],[]) = GameFinished Draw c
        round (c, rnds) ([],_)  = GameFinished Second c
        round (c, rnds) (_,[])  = GameFinished First c
        round (c, rnds) pair@((c1:c1s), (c2:c2s))
            | containsStep m rnds nextStps = GameAborted nextStps
            | otherwise = round ((c+1), rnds ++ [nextStps]) $ nextDkcs
            where
                nextStps = reverse $ fst $ gameFrRnd pair
                nextDkcs = snd $ gameFrRnd pair

safeGame_inRounds_cycleTest i = safeGame_inRounds 100 $ cyclePck i
safeGame_inRounds_casualTest i = safeGame_inRounds 100 $ cardsPck i


-- И вот второй вариант:

-- По сравнению с gameFrRnd я здесь поменял только одну строчку:
-- функцию mkPr поменял на mkThrd
gameSafeRnd :: [Rounds] -> TwoDecks -> (Rounds, TwoDecks, Bool)
gameSafeRnd rnds cardsPair = performStep ([],[]) cardsPair
    where
        revPairs [] = []
        revPairs (x:y:xs) = y:x:revPairs xs
        mkThrd stps dcks = (stps, dcks, containsStep 100 rnds stps)
        performStep (stps, acc) ([], s2) = mkThrd stps ([], s2)
        performStep (stps, acc) (s1, []) = mkThrd stps (s1, [])
        performStep (stps, acc) ((c1:c1s), (c2:c2s))
            | c1 `beats` c2 == GT = mkThrd newStps (c1s++c1:c2:reverse acc, c2s)
            | c1 `beats` c2 == LT = mkThrd newStps (c1s,c2s++c2:c1:(reverse . revPairs) acc)
            | otherwise = performStep (newStps, c2:c1:acc) (c1s, c2s)
            where
                newStps = (c1, c2):stps
                

-- И конечно же пришлось чуть-чуть поменять функцию safeGame_inRounds
safeGame_inSafeRounds :: Int -> TwoDecks -> SafeGameResult
safeGame_inSafeRounds m = round (0, [])
    where
        round (c, rnds) ([],[]) = GameFinished Draw c
        round (c, rnds) ([],_)  = GameFinished Second c
        round (c, rnds) (_,[])  = GameFinished First c
        round (c, rnds) pair@((c1:c1s), (c2:c2s))
            | thrd $ gameSafeRnd rnds pair = GameAborted nextStps
            | otherwise = round ((c+1), rnds ++ [nextStps]) $ nextDkcs
            where
                fst (f,_,_)  = f
                snd (_,s,_)  = s
                thrd (_,_,t) = t
                nextStps = reverse $ fst $ gameSafeRnd rnds pair
                nextDkcs = snd $ gameSafeRnd rnds pair

safeGame_inSafeRounds_cycleTest i = safeGame_inSafeRounds 100 $ cyclePck i
safeGame_inSafeRounds_casualTest i = safeGame_inSafeRounds 100 $ cardsPck i



{-
    Дальше пошли результаты моих психов.
    Оба предыдущих варианта мне не понравились. Как я уже сказал, они безопасны по отношению
    к повторяемости раундов. Но, между тем, возможна ситуация, когда раунды будут повторятся,
    но циклов не случится. В общем, я считаю что параметр m указанный выше немного смазанный.
    Поэтому я ради "развлечения" реализовал вариант в котором непосредственно проверяется 
    наличие m циклов следующих один за другим. Функция containsCycles - делает это.
-}

-- Лучше в нее просто не заглядывать. Она ужасна. Делает внутри много, но внешне это просто
-- проверка на повторяемость некоторой цепочки раундов в конце. Повторяемость обладается должна
-- обладать свойством последовательности - цепочки должны следовать одна за другой.
containsCycles :: (Eq a) => Int -> [a] -> (Bool, [a])
containsCycles m log
    | not $ containsStep m log (last log) = (False, [])
    | otherwise = (quantityOfCycle >=m, head logDivided)
    where
        groupInN n smt
            | length smt < n = []
            | otherwise      = take n smt : groupInN n (drop n smt)
        period = ans $ foldr count (1, True) $ init log
            where
                ans (c, f) = if f then 0 else c
                count x (acc, flag)
                    | flag && x /= last log = (acc+1, True)
                    | otherwise = (acc, False)
        logDivided = groupInN period $ reverse log
        quantityOfCycle = foldr containsRndsSer 0 $ tail logDivided
            where
                containsRndsSer x acc
                    | x == head logDivided = acc+1
                    | otherwise = acc


-- Здесь я использую containsCycles.
safeGame :: Int -> TwoDecks -> SafeGameResult
safeGame m = round (0, [])
    where
        round (c, rnds) ([],[]) = GameFinished Draw c
        round (c, rnds) ([],_)  = GameFinished Second c
        round (c, rnds) (_,[])  = GameFinished First c
        round (c, rnds) pair@((c1:c1s), (c2:c2s))
            | containsC = GameAborted (lol2l cycleRnds)
            | otherwise = round ((c+1), rnds ++ [reverse nextStps]) $ nextDkcs
            where
                (containsC, cycleRnds) = containsCycles m rnds
                (nextStps, nextDkcs) = gameFrRnd pair
                lol2l = foldr (\ x acc -> x++acc) []

safeGame_cycleTest i = safeGame 100 $ cyclePck i
safeGame_casualTest i = safeGame 100 $ cardsPck i
                

