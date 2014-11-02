module AbstractSet where

-- Если честно, то не знаю как здесь обойтись без ограничений на класс типов.
-- Этого требуют функции используемые мной в реализациях. А нужно ли вообще здесь обходиться
-- без ограничений?
class AbstractSet c where
    empty :: c t
    isEmpty :: c t -> Bool
    member :: Ord t => t -> c t -> Bool
    insert ::  Ord t => t -> c t -> c t
    delete ::  Ord t => t -> c t -> c t
    toAscList :: Ord t => c t -> [t] -- Для сравнения и проверки
