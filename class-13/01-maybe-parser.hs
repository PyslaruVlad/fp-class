import Control.Monad
import Control.Applicative

{-
   Тип Parser может быть определён следуюшим образом:
-}

newtype Parser a = Parser { apply :: String -> Maybe (a, String) }

{-
   Определите экземпляры классов Monad и MonadPlus для типа Parser в этом случае:
-}

instance Monad Parser where
  return x = Parser (\ s -> Just (x,s))
  p >>= q = Parser $ \ s0 -> case (apply p s0) of
    Nothing -> Nothing
    Just (r1, s1) -> apply (q r1) s1
  fail _ = Parser (\ _ -> Nothing)

instance MonadPlus Parser where
  mzero = Parser (\ _ -> Nothing)
  p `mplus` q = Parser (\ s -> msum $ apply <$> [p,q] <*> [s])


-- Простенькие тесты.

getc :: Parser Char
getc = Parser f
  where
    f [] = Nothing
    f (c:cs) = Just (c, cs)

testParsMonad :: Bool
testParsMonad = (jR == jT) && nothingCheck && (cT == cR)
  where
    jT = apply getc <$> ["abc", "x123", "123"]
    jR = Just <$> [('a',"bc"),('x',"123"),('1',"23")]
    nothingCheck = (apply getc "") == Nothing
    cT = flip apply "12end" $ getc >>= \ _ -> getc
    cR = Just ('2',"end")

testParsPlus :: Bool
testParsPlus = (jT == answer) && (nT == answer)
  where
    fPos = msum $ replicate 3 getc
    fNeg = mzero `mplus` getc `mplus` getc `mplus` mzero
    jT = apply fPos <$> testLists
    nT = apply fNeg <$> testLists
    testLists = ["abc", "x123", "12345"]
    answer = Just <$> [('a',"bc"),('x',"123"),('1',"2345")]
