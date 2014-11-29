{-# Language TypeSynonymInstances, FlexibleInstances, OverlappingInstances #-}
{-
   Модифицируйте представленное на лекции решение задачи о запросе пароля,
   удовлетворяющего требованиям по стойкости, следующим образом:
   - в командной строке задаются ограничения (минимальная длина, наличие букв,
     наличие цифр, наличие знаков пунктуации);
   - к стеку монад добавляется монада Reader, и с её помощью организуется
     доступ к ограничениям в функции isValid;
   - все попытки ввода пароля протоколируются средствами монады Writer.
-}

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Writer
import Control.Monad.Reader

import Data.Char
import Data.Maybe
import System.Environment

isValid :: String -> Bool
isValid s = length s >= 8 && 
                any isAlpha s && 
                any isNumber s && 
                any isPunctuation s

getValidPassword :: MaybeT IO String
getValidPassword = do
  lift $ putStrLn "Введите новый пароль:"
  s <- lift getLine
  guard (isValid s)
  return s
 
askPassword :: MaybeT IO ()
askPassword = do
  value <- msum $ repeat getValidPassword
  lift $ putStrLn "Сохранение в базе данных..."

-- main = runMaybeT askPassword


-- То, что дописал я.

type Predicate a = a -> Bool

checkingPassword :: MaybeT (ReaderT (Predicate String) (WriterT [String] IO)) String
checkingPassword = do
    liftIO $ putStr "Введите новый пароль: "
    w <- liftIO $ getLine
    tell [w]
    pred <- ask
    if pred w
    then (do
        liftIO $ putStrLn "Подходящий пароль!"
        return w)
    else (do
        liftIO $ putStrLn "Пароль не подходит :("
        mzero)

main = do
    args <- getArgs
    let 
        pred = mconcat $ parseArgs args
    (val, log) <- runWriterT $ flip runReaderT pred $ runMaybeT $ msum $ repeat checkingPassword
    putStrLn "Сохранение в базе данных..."
    putStr "Сохранение завершено. Текущий пароль: "
    print $ fromJust val
    putStr "Установлен после попыток: "
    print $ reverse log



-- Получение предиката по строке аргументов.

-- Объединение нескольких предикатов.
instance Monoid (Predicate a) where
    mappend p1 p2 = \ s -> p1 s && p2 s
    mempty = \ _ -> True

-- Чтение слов из строки аргументов и получение предикатов.
parseArgs :: [String] -> [Predicate String]
parseArgs [] = []
parseArgs ("-C":end) = (any isAlpha) : parseArgs end 
parseArgs ("-N":end) = (any isNumber) : parseArgs end 
parseArgs ("-P":end) = (any isPunctuation) : parseArgs end
parseArgs ("-M":valStr:end) = (\ s -> read valStr <= length s) : parseArgs end
parseArgs _ = error "Строка аргументов некорректна!"


        
