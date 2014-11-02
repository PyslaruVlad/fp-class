module ListSet (ListSet, empty, null, member, insert, delete) where

import AbstractSet
import qualified Data.List as L

newtype ListSet t = ListSet [t]

instance AbstractSet ListSet where
    empty = ListSet []
    isEmpty (ListSet l) = null l
    member e (ListSet l) = elem e l
    insert e (ListSet l) 
        | elem e l  = ListSet l
        | otherwise = ListSet (e:l)
    delete e (ListSet l) = ListSet (filter (/=e) l)
    toAscList (ListSet l) = L.sort l
