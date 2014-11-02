{-# LANGUAGE TypeSynonymInstances,FlexibleInstances #-}
{-
   Определите класс типов Listable с двумя функциями:
   toList :: a -> [a]
   fromList :: [a] -> a
-}

class Listable a where
    toList :: a -> [a]
    fromList :: [a] -> a

{-
  Объявите экземпляры класса типов Listable для следующих типов:
  1) String - строка разбивается по пробелам на список слов.
  2) Integer - любое целое число разбивается на список цифр.
-}

instance Listable String where
    toList = words
    fromList = unwords

instance Listable Integer where
    toList b = reverse $ modDiv b
        where
            modDiv 0 = []
            modDiv a = mod a 10 : (modDiv $ div a 10)
    fromList = fst . foldr (\ n (s, m) -> (n*m + s, m*10)) (0, 1)



testStrings :: Int -> Bool
testStrings 1 = fromList (toList "   word1 word2 word3   .") == "word1 word2 word3 ."
testStrings 2 = fromList ["word1", "word 2"] == "word1 word 2"
testStrings 3 = toList "word1 word2" == ["word1", "word2"]
testStrings _ = error "Такого теста я не придумывал!"


testInts :: Int -> Bool
testInts 1 = fromList (toList 12345) == (12345::Integer)
testInts 2 = toList 012340 == ([1,2,3,4,0]::[Integer])
testInts 3 = fromList ([0,1,2,3,4,0,5]::[Integer]) == 123405
testInts _ = error "Такого теста я не придумывал!"
