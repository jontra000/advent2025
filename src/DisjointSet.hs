module DisjointSet (DisjointSet, add, find, union) where

import qualified Data.Map as M

newtype DisjointSet a = DisjointSet (M.Map a a)

add :: Ord a => a -> DisjointSet a -> DisjointSet a
add x djs@(DisjointSet impl)
    | M.member x impl = djs
    | otherwise = DisjointSet $ M.insert x x impl

find :: Ord a => a -> DisjointSet a -> a
find x djs@(DisjointSet impl)
    | parent == x = x
    | otherwise = find parent djs
    where parent = impl M.! x

union :: Ord a => a -> a -> DisjointSet a -> DisjointSet a
union x y djs@(DisjointSet impl)
    | parentX == parentY = djs
    | otherwise = DisjointSet $ M.insert parentX parentY impl
    where parentX = find x djs
          parentY = find y djs