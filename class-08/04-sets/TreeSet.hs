{-# LANGUAGE FlexibleInstances #-}

module TreeSet (TreeSet, empty, null, member, insert, delete) where

import AbstractSet

type TreeSet t = Tree t -- просто везде в 04-sets я уже писал с TreeSet
data Tree t = Tree t (Tree t) (Tree t) | EmptyTree deriving (Eq)

instance AbstractSet Tree where

    empty = EmptyTree
    
    isEmpty EmptyTree = True
    isEmpty _ = False
    
    member _ EmptyTree = False
    member e (Tree root left right)
        | e < root = member e left
        | e > root = member e right
        | otherwise = True
    
    insert e EmptyTree = Tree e EmptyTree EmptyTree
    insert e init@(Tree root left right)
        | e < root = Tree root (insert e left) right
        | e > root = Tree root left (insert e right)
        | otherwise = init
    
    delete _ EmptyTree = EmptyTree
    delete e (Tree root left right)
        | e < root = Tree root (delete e left) right
        | e > root = Tree root left (delete e right)
        | otherwise = append left right
        where
            append left EmptyTree = left
            append left (Tree root EmptyTree nextR) = Tree root left nextR
            append left (Tree root nextL nextR) = Tree root (append left nextL) nextR
    
    toAscList EmptyTree = []
    toAscList (Tree root left right) = (toAscList left) ++ [root] ++ (toAscList right)
