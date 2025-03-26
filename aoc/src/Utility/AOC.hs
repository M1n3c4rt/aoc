{-|
Module      : AOC
Description : Utility functions commonly used while solving Advent of Code puzzles
Copyright   : (c) M1n3c4rt, 2025
License     : BSD-3-Clause
Maintainer  : vedicbits@gmail.com
Stability   : stable
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Utility.AOC (
    -- * Pathfinding algorithms
    -- $cat1
    shortestDistance,
    shortestPaths,
    shortestDistanceOnMagma,
    shortestPathsOnMagma,
    -- * Neighbour functions
    neighbours4,
    neighbours8,
    neighbours6,
    neighbours26,
    -- * Taxicab (Manhattan) distance
    taxicab2,
    taxicab3,
    -- * Grid enumeration
    -- $cat2
    enumerate,
    enumerateRead,
    enumerateHM,
    enumerateReadHM,
    enumerateFilter,
    enumerateFilterSet,
    -- * Flood fill
    floodFill,
    floodFillWith,
    -- * List selection
    choose,
    permute,
    -- * Extrapolation
    extrapolate,
    -- * Miscellaneous
    range,
    rangeIntersect,
    binToDec,
    -- Export types/constraints used in top-level function type signatures
    Hashable,
    HM.HashMap,
    S.Set
) where

import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import qualified Data.Set as S
import qualified Data.Heap as H
import Data.List (permutations, genericIndex)

createMinPrioHeap :: Ord a1 => (a1,a) -> H.MinPrioHeap a1 a
createMinPrioHeap = H.singleton

-- $cat1
-- All of the following functions return distances as a @Maybe Int@, where @Nothing@ is returned if no path is found.
-- The graph is a @HashMap@ mapping each node to a sequence of (neighbour, edge weight) pairs.

-- | Returns the shortest distance between two nodes in a graph.
shortestDistance :: (Foldable t, Hashable n, Ord a, Num a)
    => HM.HashMap n (t (n, a)) -- ^ Graph
    -> n -- ^ Start node
    -> n -- ^ End node
    -> Maybe a
shortestDistance graph = shortestDistanceOnMagma (repeat graph)

-- | Returns the shortest distance between two nodes in a graph and a list of all possible paths from the ending node to the starting node.
-- The starting node is not included in each path.
shortestPaths :: (Foldable t, Hashable n, Ord a, Num a)
    => HM.HashMap n (t (n, a)) -- ^ Graph
    -> n -- ^ Start node
    -> n -- ^ End node
    -> (Maybe a, [[n]])
shortestPaths graph = shortestPathsOnMagma (repeat graph)

-- | Returns the shortest distance between two nodes in a list of graphs where the neighbours of a node in any given graph all lie in the succeeding graph. The ending node must be present in each graph.
-- This precondition is not checked.
shortestDistanceOnMagma :: (Foldable t, Hashable n, Ord a, Num a)
    => [HM.HashMap n (t (n, a))] -- ^ Graphs
    -> n -- ^ Start node
    -> n -- ^ End node
    -> Maybe a
shortestDistanceOnMagma graphs start end = fst $ shortestPathsOnMagma graphs start end

-- | Returns the shortest distance between two nodes in a list of graphs and a list of all possible paths from the ending node to the starting node. The ending node must be present in each graph.
-- This precondition is not checked.
-- The starting node is not included in each path.
shortestPathsOnMagma :: (Foldable t, Hashable n, Ord a, Num a)
    => [HM.HashMap n (t (n, a))] -- ^ Graphs
    -> n -- ^ Start node
    -> n -- ^ End node
    -> (Maybe a, [[n]])
shortestPathsOnMagma graphs start end =
    let initQueue = createMinPrioHeap (0,start)
        initPaths = HM.singleton start (0,[[]])
        helper (paths,queue) = case H.view queue of
            Nothing -> (paths,queue)
            Just ((_,n),ns) ->
                let Just (currentDistance,currentPaths) = HM.lookup n paths
                    Just neighbours = HM.lookup n (graphs !! length (head currentPaths))
                    updateNeighbour (n',d') (p',q') = case HM.lookup n' p' of
                        Nothing -> (HM.insert n' (currentDistance+d',map (n':) currentPaths) p', H.insert (currentDistance+d',n') q')
                        Just (d'',ps'') ->
                            if d'' < currentDistance+d' then
                                (p',q')
                            else if d'' > currentDistance+d' then
                                (HM.insert n' (currentDistance+d',map (n':) currentPaths) p', H.insert (currentDistance+d',n') q')
                            else
                                (HM.insert n' (currentDistance+d',ps'' ++ map (n':) currentPaths) p', q')
                in helper $ foldr updateNeighbour (paths,ns) neighbours

    in case HM.lookup end $ fst (helper (initPaths,initQueue)) of
        Nothing -> (Nothing, [])
        Just (d,ps) -> (Just d, ps)

-- | Returns the 4 points orthogonally adjacent to the given point.
neighbours4 :: (Num a, Num b) => (a, b) -> [(a, b)]
neighbours4 (x,y) = [(x+1,y),(x,y+1),(x-1,y),(x,y-1)]

-- | Returns the 8 points orthogonally or diagonally adjacent to the given point.
neighbours8 :: (Enum a, Enum b, Eq a, Eq b, Num a, Num b) => (a, b) -> [(a, b)]
neighbours8 (x,y) = [(x+p,y+q) | p <- [-1..1], q <- [-1..1], p /= 0 || q /= 0]

-- | Returns the 6 points orthogonally adjacent to the given point in 3D space.
neighbours6 :: (Num a, Num b, Num c) => (a, b, c) -> [(a, b, c)]
neighbours6 (x,y,z) = [(x+1,y,z),(x,y+1,z),(x,y,z+1),(x-1,y,z),(x,y-1,z),(x,y,z-1)]

-- | Returns the 26 points orthogonally or diagonally adjacent to the given point in 3D space.
neighbours26 :: (Enum a, Enum b, Enum c, Eq a, Eq b, Eq c, Num a, Num b, Num c) => (a, b, c) -> [(a, b, c)]
neighbours26 (x,y,z) = [(x+p,y+q,z+r) | p <- [-1..1], q <- [-1..1], r <- [-1..1], p /= 0 || q /= 0 || r /= 0]

-- | Returns the Taxicab/Manhattan distance between two points in 2D space.
taxicab2 :: Num a => (a, a) -> (a, a) -> a
taxicab2 (a,b) (c,d) = abs (a-c) + abs (b-d)

-- | Returns the Taxicab/Manhattan distance between two points in 3D space.
taxicab3 :: Num a => (a, a, a) -> (a, a, a) -> a
taxicab3 (a,b,c) (d,e,f) = abs (a-d) + abs (b-e) + abs (c-f)

enumerateBase :: (Num y, Num x, Enum y, Enum x) => String -> [((x, y), Char)]
enumerateBase s =
    let ss = lines s
        ys = zipWith (\n l -> map (n,) l) [0..] ss
        xs = map (zipWith (\x (y,c) -> ((x,y),c)) [0..]) ys
    in concat xs

-- $cat2
-- The following functions operate on a grid of characters as a string with a newline after each row (as seen in several Advent of Code puzzle inputs).

-- | Converts a grid to a list of triples @(x,y,c)@ representing xy coordinates and the character at that location.
enumerate :: (Num y, Num x, Enum y, Enum x) => String -> [(x, y, Char)]
enumerate = map (\((x,y),c) -> (x,y,c)) . enumerateBase

-- | Enumerates a grid along with reading the characters (usually as integers).
enumerateRead :: (Read c, Num y, Num x, Enum y, Enum x) => String -> [(x, y, c)]
enumerateRead = map (\((x,y),c) -> (x,y,read [c])) . enumerateBase

-- | Enumerates a grid and stores it in a @HashMap@ where points are mapped to the character at that location.
enumerateHM :: (Num x, Num y, Enum x, Enum y, Hashable x, Hashable y) => String -> HM.HashMap (x, y) Char
enumerateHM = HM.fromList . enumerateBase

-- | Enumerates a grid and stores it in a @HashMap@ along with reading the characters (usually as integers).
enumerateReadHM :: (Num x, Num y, Enum x, Enum y, Hashable x, Hashable y, Read c) => String -> HM.HashMap (x, y) c
enumerateReadHM = HM.fromList . map (\((x,y),c) -> ((x,y),read [c])) . enumerateBase

-- | Returns a list of points on a grid for which a certain condition is met.
enumerateFilter :: (Num y, Num x, Enum y, Enum x) => (Char -> Bool) -> String -> [(x, y)]
enumerateFilter f = map fst . filter (f . snd) . enumerateBase

-- | Returns a set of points on a grid for which a certain condition is met.
enumerateFilterSet :: (Ord x, Ord y, Num y, Num x, Enum y, Enum x) => (Char -> Bool) -> String -> S.Set (x, y)
enumerateFilterSet f = S.fromList . enumerateFilter f

floodFill' :: Ord a => (a -> [a]) -> S.Set a -> S.Set a -> S.Set a -> S.Set a
floodFill' neighbours finished frontier blocks
    | S.null frontier = finished
    | otherwise = floodFill' neighbours (S.union frontier finished) newfrontier blocks
    where
        newfrontier = S.filter (\n -> n `S.notMember` finished || n `S.notMember` frontier || n `S.notMember` blocks) $ S.unions $ S.map (S.fromList . neighbours) frontier

floodFillWith' :: Ord a => (a -> a -> Bool) -> (a -> [a]) -> S.Set a -> S.Set a -> S.Set a
floodFillWith' cond neighbours finished frontier
    | S.null frontier = finished
    | otherwise = floodFillWith' cond neighbours (S.union frontier finished) newfrontier
    where
        newfrontier = S.filter (\n -> n `S.notMember` finished || n `S.notMember` frontier) $ S.unions $ S.map (S.fromList . (\c -> filter (cond c) $ neighbours c)) frontier

-- | Applies a flood fill algorithm given a function to generate a point's neighbours, a starting set of points, and a set of points to avoid. Returns a set of all points covered.
floodFill :: Ord a
    => (a -> [a]) -- ^ Neighbour function
    -> S.Set a -- ^ Initial set of points
    -> S.Set a -- ^ Set of points to avoid
    -> S.Set a
floodFill neighbours = floodFill' neighbours S.empty

-- | Applies a flood fill algorithm given a function to generate a point's neighbours, a condition that filters out points generated by said function, and a starting set of points. Returns a set of all points covered.
-- The condition is of the form @a -> a -> Bool@, which returns @True@ if the second point is a valid neighbour of the first point and @False@ otherwise.
floodFillWith :: Ord a
    => (a -> a -> Bool) -- ^ Condition
    -> (a -> [a]) -- ^ Neighbour function
    -> S.Set a -- ^ Initial set of points
    -> S.Set a
floodFillWith cond neighbours = floodFillWith' cond neighbours S.empty

-- | Generates a list of all possible lists of length n by taking elements from the provided list of length l.
-- Relative order is maintained, and the length of the returned list is \(_{n}C_{l}\).
choose :: (Num n, Eq n) => n -> [a] -> [[a]]
choose n (x:xs) = map (x:) (choose (n-1) xs) ++ choose n xs
choose 0 _ = [[]]
choose _ [] = []

-- | Generates a list of all possible lists of length n by taking elements from the provided list of length l.
-- The length of the returned list is \(_{n}P_{l}\).
permute :: (Num n, Eq n) => n -> [a] -> [[a]]
permute n = concatMap permutations . choose n

-- | Gets the nth element of an infinite list, assuming that each element in the list can be generated using the previous element, for example, a list generated with @iterate@.
extrapolate :: (Integral b, Ord a) => b -> [a] -> a
extrapolate n ls = let (o,p) = helper 0 S.empty ls in ls `genericIndex` (((n-o) `mod` p) + o)
    where
        helper k finished (l:ls')
            | S.null matches = helper (k+1) (S.insert (k,l) finished) ls'
            | otherwise = let o = fst $ S.elemAt 0 matches in (o,k-o)
            where matches = S.filter ((==l) . snd) finished

-- | Generates a range with @[x..y]@, but reverses the list instead of returning an empty range if x > y.
range :: (Ord a, Enum a) => a -> a -> [a]
range x y = if y < x then [x,pred x..y] else [x..y]

-- | Takes (a,b) and (c,d) as arguments and returns the intersection of the ranges [a..b] and [c..d] as another pair if it is not empty.
rangeIntersect :: Ord b => (b, b) -> (b, b) -> Maybe (b, b)
rangeIntersect (a,b) (c,d)
    | b < c || a > d = Nothing
    | otherwise = Just (max a c, min b d)

-- | Converts a list of booleans (parsed as a binary number) to an integer.
binToDec :: Num a => [Bool] -> a
binToDec = sum . zipWith (*) (map (2^) [0..]) . map (fromIntegral . fromEnum) . reverse