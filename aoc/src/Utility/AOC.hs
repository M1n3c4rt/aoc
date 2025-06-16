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
    shortestDistanceWith,
    shortestPathsWith,
    -- * Neighbour functions
    neighbours4,
    neighbours8,
    neighbours6,
    neighbours26,
    -- * Taxicab (Manhattan) distance
    taxicab2,
    taxicab3,
    taxicab,
    -- * Input parsing
    -- $cat2
    enumerate',
    enumerateRead',
    enumerate,
    enumerateRead,
    enumerateHM,
    enumerateReadHM,
    enumerateFilter,
    enumerateFilterSet,
    numbers,
    numbers',
    -- * Flood fill
    floodFill,
    floodFillWith,
    -- * List selection
    choose,
    permute,
    takeEvery,
    chunk,
    -- * Extrapolation
    extrapolate,
    -- * Debugging
    prettyPrintSet,
    prettyPrintSetWide,
    prettyPrintHM,
    prettyPrintHMWide,
    traceSleep,
    traceSleepSeconds,
    -- * Memoization
    -- $cat3
    memo2,
    memo3,
    memo4,
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
import Data.List (permutations, genericIndex, groupBy)
import Data.Maybe (fromMaybe)
import System.IO.Unsafe (unsafePerformIO)
import Control.Concurrent (threadDelay, newMVar, readMVar, modifyMVar_)
import Data.Function (on)

createMinPrioHeap :: Ord a1 => (a1,a) -> H.MinPrioHeap a1 a
createMinPrioHeap = H.singleton

-- $cat1
-- All of the following functions return distances as a @Maybe Int@, where @Nothing@ is returned if no path is found.
-- The graph is a @HashMap@ mapping each node to a sequence of (neighbour, edge weight) pairs.
-- Functions that take a graph assume that any neighbour that a node in the graph points to is also in the graph.

-- | Returns the shortest distance between two nodes in a graph.
shortestDistance :: (Foldable t, Hashable n, Ord a, Num a)
    => HM.HashMap n (t (n, a)) -- ^ Graph
    -> n -- ^ Start node
    -> n -- ^ End node
    -> Maybe a
shortestDistance graph = shortestDistanceWith (\n -> fromMaybe (error "Node not in graph") $ HM.lookup n graph)

-- | Returns the shortest distance between two nodes in a graph and a list of all possible paths from the ending node to the starting node.
-- The starting node is not included in each path.
shortestPaths :: (Foldable t, Hashable n, Ord a, Num a)
    => HM.HashMap n (t (n, a)) -- ^ Graph
    -> n -- ^ Start node
    -> n -- ^ End node
    -> (Maybe a, [[n]])
shortestPaths graph = shortestPathsWith (\n -> fromMaybe (error "Node not in graph") $ HM.lookup n graph)

-- | Given a function that takes a node and returns a sequence of (neighbour,edge weight) pairs, returns the shortest distance between two nodes.
shortestDistanceWith :: (Foldable t, Hashable n, Ord a, Num a)
    => (n -> t (n, a)) -- ^ Function to generate neighbours
    -> n -- ^ Start node
    -> n -- ^ End node
    -> Maybe a
shortestDistanceWith f start end = fst $ shortestPathsWith f start end

-- | Given a function that takes a node and returns a sequence of (neighbour,edge weight) pairs, returns the shortest distance between two nodes and a list of all possible paths from the ending node to the starting node.
-- The starting node is not included in each path.
shortestPathsWith :: (Foldable t, Hashable n, Ord a, Num a)
    => (n -> t (n, a)) -- ^ Function to generate neighbours
    -> n -- ^ Start node
    -> n -- ^ End node
    -> (Maybe a, [[n]])
shortestPathsWith f start end =
    let initQueue = createMinPrioHeap (0,start)
        initPaths = HM.singleton start (0,[[]])
        helper (paths,queue) = case H.view queue of
            Nothing -> (paths,queue)
            Just ((_,n),ns) ->
                let Just (currentDistance,currentPaths) = HM.lookup n paths
                    neighbours = f n
                    updateNeighbour (n',d') (p',q') = case HM.lookup n' p' of
                        Nothing -> (HM.insert n' (currentDistance+d',map (n':) currentPaths) p', H.insert (currentDistance+d',n') $ H.filter ((/=n') . snd) q')
                        Just (d'',ps'') ->
                            if d'' < currentDistance+d' then
                                (p',q')
                            else if d'' > currentDistance+d' then
                                (HM.insert n' (currentDistance+d',map (n':) currentPaths) p', H.insert (currentDistance+d',n') $ H.filter ((/=n') . snd) q')
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
neighbours8 :: (Eq a, Eq b, Num a, Num b) => (a, b) -> [(a, b)]
neighbours8 (x,y) = [(x+p,y+q) | p <- [-1,0,1], q <- [-1,0,1], p /= 0 || q /= 0]

-- | Returns the 6 points orthogonally adjacent to the given point in 3D space.
neighbours6 :: (Num a, Num b, Num c) => (a, b, c) -> [(a, b, c)]
neighbours6 (x,y,z) = [(x+1,y,z),(x,y+1,z),(x,y,z+1),(x-1,y,z),(x,y-1,z),(x,y,z-1)]

-- | Returns the 26 points orthogonally or diagonally adjacent to the given point in 3D space.
neighbours26 :: (Eq a, Eq b, Eq c, Num a, Num b, Num c) => (a, b, c) -> [(a, b, c)]
neighbours26 (x,y,z) = [(x+p,y+q,z+r) | p <- [-1,0,1], q <- [-1,0,1], r <- [-1,0,1], p /= 0 || q /= 0 || r /= 0]

-- | Returns the Taxicab/Manhattan distance between two points in 2D space.
taxicab2 :: Num a => (a, a) -> (a, a) -> a
taxicab2 (a,b) (c,d) = abs (a-c) + abs (b-d)

-- | Returns the Taxicab/Manhattan distance between two points in 3D space.
taxicab3 :: Num a => (a, a, a) -> (a, a, a) -> a
taxicab3 (a,b,c) (d,e,f) = abs (a-d) + abs (b-e) + abs (c-f)

-- | Returns the Taxicab/Manhattan distance between two points in n dimensions, where both points are lists of length n.
taxicab :: Num a => [a] -> [a] -> a
taxicab as bs = sum $ zipWith (\x y -> abs (x-y)) as bs

-- $cat2
-- The following functions (beginning with "enumerate") operate on a grid of characters as a string with a newline after each row (as seen in several Advent of Code puzzle inputs).

-- | Converts a grid to a list of pairs @((x,y),c)@ representing xy coordinates and the character at that location.
enumerate' :: (Num y, Num x) => String -> [((x, y), Char)]
enumerate' s =
    let ss = lines s
        ys = zipWith (\n l -> map (n,) l) (iterate (+1) 0) ss
        xs = map (zipWith (\x (y,c) -> ((x,y),c)) (iterate (+1) 0)) ys
    in concat xs

-- | Enumerates a grid along with reading the characters (usually as integers), and returns a list of pairs.
enumerateRead' :: (Read c, Num y, Num x) => String -> [((x, y), c)]
enumerateRead' = map (\((x,y),c) -> ((x,y),read [c])) . enumerate'

-- | Converts a grid to a list of triples @(x,y,c)@ representing xy coordinates and the character at that location.
enumerate :: (Num y, Num x) => String -> [(x, y, Char)]
enumerate = map (\((x,y),c) -> (x,y,c)) . enumerate'

-- | Enumerates a grid along with reading the characters (usually as integers), and returns a list of triples.
enumerateRead :: (Read c, Num y, Num x) => String -> [(x, y, c)]
enumerateRead = map (\((x,y),c) -> (x,y,read [c])) . enumerate'

-- | Enumerates a grid and stores it in a @HashMap@ where points are mapped to the character at that location.
enumerateHM :: (Num x, Num y, Enum x, Enum y, Hashable x, Hashable y) => String -> HM.HashMap (x, y) Char
enumerateHM = HM.fromList . enumerate'

-- | Enumerates a grid and stores it in a @HashMap@ along with reading the characters (usually as integers).
enumerateReadHM :: (Num x, Num y, Enum x, Enum y, Hashable x, Hashable y, Read c) => String -> HM.HashMap (x, y) c
enumerateReadHM = HM.fromList . map (\((x,y),c) -> ((x,y),read [c])) . enumerate'

-- | Returns a list of points on a grid for which a certain condition is met.
enumerateFilter :: (Num y, Num x) => (Char -> Bool) -> String -> [(x, y)]
enumerateFilter f = map fst . filter (f . snd) . enumerate'

-- | Returns a set of points on a grid for which a certain condition is met.
enumerateFilterSet :: (Ord x, Ord y, Num y, Num x) => (Char -> Bool) -> String -> S.Set (x, y)
enumerateFilterSet f = S.fromList . enumerateFilter f

-- | Returns all the integers in a string (including negative signs).
numbers :: (Num a, Read a) => [Char] -> [a]
numbers = map read . filter (isDigit . head) . groupBy ((==) `on` isDigit)
    where isDigit = (`elem` "1234567890-")

-- | Returns all the integers in a string (excluding negative signs).
numbers' :: (Num a, Read a) => [Char] -> [a]
numbers' = map read . filter (isDigit . head) . groupBy ((==) `on` isDigit)
    where isDigit = (`elem` "1234567890")

floodFill' :: Ord a => (a -> [a]) -> S.Set a -> [a] -> S.Set a -> S.Set a
floodFill' neighbours finished (f:frontier) blocks = floodFill' neighbours (S.insert f finished) (frontier++filtered) blocks
    where filtered = filter (\n -> n `S.notMember` finished && n `notElem` frontier && n `S.notMember` blocks) $ neighbours f
floodFill' _ finished [] _ = finished

floodFillWith' :: Ord a => (a -> a -> Bool) -> (a -> [a]) -> S.Set a -> [a] -> S.Set a
floodFillWith' cond neighbours finished (f:frontier) = floodFillWith' cond neighbours (S.insert f finished) (frontier++filtered)
    where filtered = filter (\n -> n `S.notMember` finished && n `notElem` frontier && cond f n) $ neighbours f
floodFillWith' _ _ finished [] = finished

-- | Applies a flood fill algorithm given a function to generate a point's neighbours, a starting set of points, and a set of points to avoid. Returns a set of all points covered.
floodFill :: Ord a
    => (a -> [a]) -- ^ Neighbour function
    -> S.Set a -- ^ Initial set of points
    -> S.Set a -- ^ Set of points to avoid
    -> S.Set a
floodFill neighbours frontier = floodFill' neighbours S.empty (S.toList frontier)

-- | Applies a flood fill algorithm given a function to generate a point's neighbours, a condition that filters out points generated by said function, and a starting set of points. Returns a set of all points covered.
-- The condition is of the form @a -> a -> Bool@, which returns @True@ if the second point is a valid neighbour of the first point and @False@ otherwise.
floodFillWith :: Ord a
    => (a -> a -> Bool) -- ^ Condition
    -> (a -> [a]) -- ^ Neighbour function
    -> S.Set a -- ^ Initial set of points
    -> S.Set a
floodFillWith cond neighbours frontier = floodFillWith' cond neighbours S.empty (S.toList frontier)

-- | Generates a list of all possible lists of length n by taking elements from the provided list of length l.
-- Relative order is maintained, and the length of the returned list is \(_{n}C_{l}\).
choose :: (Num n, Ord n) => n -> [a] -> [[a]]
choose 0 _ = [[]]
choose _ [] = []
choose n (x:xs)
    | n > fromIntegral (length (x:xs)) = []
    | otherwise = map (x:) (choose (n-1) xs) ++ choose n xs

-- | Generates a list of all possible lists of length n by taking elements from the provided list of length l.
-- The length of the returned list is \(_{n}P_{l}\).
permute :: (Num n, Ord n) => n -> [a] -> [[a]]
permute n = concatMap permutations . choose n

-- | Takes every nth element from a list xs, starting from @xs !! (n-1)@.
takeEvery :: Int -> [a] -> [a]
takeEvery _ [] = []
takeEvery n xs = let (a,b) = splitAt n xs in if length a < n then [] else last a:takeEvery n b

-- | Splits a list into sublists of size n. The length of the last sublist may be less than n.
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = let (a,b) = splitAt n xs in a:chunk n b

-- | Gets the nth element of an infinite list, assuming that each element in the list can be generated using the previous element, for example, a list generated with @iterate@.
extrapolate :: (Integral b, Ord a) => b -> [a] -> a
extrapolate n ls = let (o,p) = helper 0 S.empty ls in ls `genericIndex` (((n-o) `mod` p) + o)
    where
        helper k finished (l:ls')
            | S.null matches = helper (k+1) (S.insert (k,l) finished) ls'
            | otherwise = let o = fst $ S.elemAt 0 matches in (o,k-o)
            where matches = S.filter ((==l) . snd) finished

-- | Converts a set of points @(x,y)@ to a string composed of @'#'@ and @' '@. This function is useful when displaying puzzle answers formed by a grid of points.
-- Up to translation of points, @prettyPrintSet . enumerateFilterSet (=='#') = id@.
prettyPrintSet :: (Enum b, Enum a, Ord a, Ord b) => S.Set (a, b) -> String
prettyPrintSet points = unlines [[if (x,y) `S.member` points then '#' else ' ' | x <- [xmin..xmax]] | y <- reverse [ymin..ymax]]
    where
        xs = S.map fst points
        ys = S.map snd points
        (xmin,xmax,ymin,ymax) = (minimum xs, maximum xs, minimum ys, maximum ys)

-- | Same as @prettyPrintSet@, but displays points at double width to improve readability.
prettyPrintSetWide :: (Enum b, Enum a, Ord a, Ord b) => S.Set (a, b) -> String
prettyPrintSetWide = foldr (\c acc -> if c /= '\n' then c:c:acc else c:acc) [] . prettyPrintSet

-- | Converts a @HashMap@ of points @(x,y)@ and characters @c@ to a string with the corresponding character at each point. This function is useful when displaying puzzle answers formed by a grid of points.
-- Up to translation of points, @prettyPrintHM . enumerateHM = id@.
prettyPrintHM :: (Enum b, Enum a, Hashable a, Hashable b, Ord a, Ord b) => HM.HashMap (a, b) Char -> String
prettyPrintHM points = unlines [[HM.lookupDefault ' ' (x,y) points | x <- [xmin..xmax]] | y <- reverse [ymin..ymax]]
    where
        xs = map fst $ HM.keys points
        ys = map snd $ HM.keys points
        (xmin,xmax,ymin,ymax) = (minimum xs, maximum xs, minimum ys, maximum ys)

-- | Same as @prettyPrintHM@, but displays points at double width to improve readability.
prettyPrintHMWide :: (Enum b, Enum a, Hashable a, Hashable b, Ord a, Ord b) => HM.HashMap (a, b) Char -> String
prettyPrintHMWide = foldr (\c acc -> if c /= '\n' then c:c:acc else c:acc) [] . prettyPrintHM

{-# NOINLINE traceSleep #-}
-- | Pauses execution for n microseconds, before returning the second argument as its result. Useful for slowing down output that normally floods the terminal.
-- Like functions exported by Debug.Trace, this function should only be used for debugging.
-- The function is not referentially transparent: its type indicates that it is a pure function but it has the side effect of delaying execution.
traceSleep :: Int -> a -> a
traceSleep n x = unsafePerformIO $ do
    threadDelay n
    return x

{-# NOINLINE traceSleepSeconds #-}
-- | Pauses execution for n seconds. See @traceSleep@.
traceSleepSeconds :: Int -> a -> a
traceSleepSeconds n = traceSleep (n*1000000)

-- $cat3
-- Memoize a function with multiple arguments. Uses @memo@ from @Data.MemoUgly@ with slight modifications.

memo2 :: (Hashable a, Hashable b) => (a -> b -> c) -> (a -> b -> c)
memo2 f = unsafePerformIO $ do
    v <- newMVar HM.empty
    let f' a b = unsafePerformIO $ do
            m <- readMVar v
            case HM.lookup (a,b) m of
                Nothing -> do let { r = f a b }; modifyMVar_ v (return . HM.insert (a,b) r); return r
                Just r  -> return r
    return f'
memo3 :: (Hashable a, Hashable b, Hashable c) => (a -> b -> c -> d) -> (a -> b -> c -> d)
memo3 f = unsafePerformIO $ do
    v <- newMVar HM.empty
    let f' a b c = unsafePerformIO $ do
            m <- readMVar v
            case HM.lookup (a,b,c) m of
                Nothing -> do let { r = f a b c }; modifyMVar_ v (return . HM.insert (a,b,c) r); return r
                Just r  -> return r
    return f'
memo4 :: (Hashable a, Hashable b, Hashable c, Hashable d) => (a -> b -> c -> d -> e) -> (a -> b -> c -> d -> e)
memo4 f = unsafePerformIO $ do
    v <- newMVar HM.empty
    let f' a b c d = unsafePerformIO $ do
            m <- readMVar v
            case HM.lookup (a,b,c,d) m of
                Nothing -> do let { r = f a b c d }; modifyMVar_ v (return . HM.insert (a,b,c,d) r); return r
                Just r  -> return r
    return f'

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