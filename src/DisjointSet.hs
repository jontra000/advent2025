{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module DisjointSet (DisjointSet, add, findRoot, fromList, union, groupSizes) where

import qualified Data.Map as M

data Node a = Node (Maybe a) Int deriving Show -- parent, size 
newtype DisjointSet a = DisjointSet (M.Map a (Node a)) deriving Show

add :: Ord a => a -> DisjointSet a -> DisjointSet a
add x djs@(DisjointSet impl')
    | M.member x impl' = djs
    | otherwise = DisjointSet $ M.insert x (Node Nothing 1) impl'

fromList :: Ord a => [a] -> DisjointSet a
fromList = DisjointSet . M.fromList . map (\x -> (x, Node Nothing 1))

findRoot :: Ord a => a -> DisjointSet a -> a
findRoot x djs@(DisjointSet impl') = 
    case impl' M.! x of
      (Node Nothing _) -> x
      (Node (Just parent) _) -> findRoot parent djs

size :: Ord a => a -> DisjointSet a -> Int
size x (DisjointSet impl') = nodeSize $ impl' M.! x

union :: Ord a => a -> a -> DisjointSet a -> DisjointSet a
union x y djs@(DisjointSet impl')
    | rootX == rootY = djs
    | otherwise =
        let sizeX = size rootX djs
            sizeY = size rootY djs
            newParentNode = Node Nothing (sizeX + sizeY)
            nodeX = if sizeX > sizeY then newParentNode else Node (Just rootY) sizeX
            nodeY = if sizeX > sizeY then Node (Just rootX) sizeY else newParentNode
            impl'' = M.insert rootY nodeY $ M.insert rootX nodeX impl'
        in  DisjointSet impl''
    where rootX = findRoot x djs
          rootY = findRoot y djs

groupSizes :: DisjointSet a -> [Int]
groupSizes = map nodeSize . filter isRoot . M.elems . impl

impl :: DisjointSet a -> M.Map a (Node a)
impl (DisjointSet impl') = impl'

isRoot :: Node a -> Bool
isRoot (Node Nothing _) = True
isRoot _ = False

nodeSize :: Node a -> Int
nodeSize (Node _ size') = size'