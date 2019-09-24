{-# LANGUAGE Strict #-}
module Main where
-- Projeto 1 - MC346 Paradigmas de programação - 2S2019
-- Júlio Moreira Blas de Barros | RA 200491
-- Gabriel da Silva Costa       | RA 216158

import qualified Data.Map.Strict as StrictMap
import Data.List
import System.IO (isEOF)
type Map = StrictMap.Map

sq a = a * a

data Point = Point [Char] [Double] deriving (Show, Eq)
getLabel (Point label _) = label

dist (Point _ p1) (Point _ p2) = sqrt (sum $ map (\(a, b) -> sq (a - b)) $ zip p1 p2)

splitStr list delimiter = splitStr' [] [] list delimiter
    where splitStr' :: Eq a => [[a]] -> [a] -> [a] -> a -> [[a]]
          splitStr' finalAcc [] [] delimiter = finalAcc
          splitStr' finalAcc acc [] delimiter = finalAcc++[acc]
          splitStr' finalAcc acc (x:xs) delimiter
            | x == delimiter && acc /= [] = splitStr' (finalAcc++[acc]) [] xs delimiter
            | x == delimiter = splitStr' finalAcc [] xs delimiter
            | otherwise = splitStr' finalAcc (acc++[x]) xs delimiter

createPointFromLine :: [Char] -> Point
createPointFromLine line = Point (head tokens) (map read $ tail tokens)
    where tokens = splitStr line ' '

attachLabel :: Map [Char] [Point] -> [Char] -> Point -> Map [Char] [Point]
attachLabel aMap label point
    | StrictMap.member label aMap = StrictMap.adjust (\v -> point:v) label aMap
    | otherwise = StrictMap.insert label [point] aMap


getDist :: ([Char], [Point], Double, Point) -> Double
getDist (_, _, d, _) = d

simplify :: ([Char], [Point], Double, Point) -> ([Char], Point)
simplify (label, _, _, p) = (label, p)

minimumDistance :: [Point] -> Point -> Double
minimumDistance [] _ = 0
minimumDistance points p = dist (minimumBy (\p1 -> \p2 -> compare (dist p p1) (dist p p2)) points) p

minimumTuple :: [([Char], [Point])] -> Point -> ([Char], [Point], Double)
minimumTuple pointTuples p =
    minimumBy (\(_, _, d1) -> \(_, _, d2) -> compare d1 d2) pointTupleDistances
    where pointTupleDistances = map (\(label, ps1) -> (label, ps1, (minimumDistance ps1 p))) pointTuples

getNearestLabel :: Map [Char] [Point] -> Point -> ([Char], [Point], Double)
getNearestLabel aMap p = minimumTuple (StrictMap.assocs aMap) p

getNearestPointAndLabel :: Map [Char] [Point] -> [Point] -> ([Char], Point)
getNearestPointAndLabel aMap ps =
    simplify $ minimumBy (\p1 -> \p2 -> compare (getDist p1) (getDist p2)) distances
    where distances = map (\((a,b,c),d) -> (a,b,c,d)) $ map (\p -> ((getNearestLabel aMap p),p)) ps

enrich :: Map [Char] [Point] -> [Point] -> Map [Char] [Point]
enrich aMap [] = aMap
enrich aMap points =
    let (label, point) = (getNearestPointAndLabel aMap points) in
        enrich (attachLabel aMap label point) (delete point points)

getLineLabelTuple :: [Char] -> ([Char], [[Char]])
getLineLabelTuple line =
    let tokens = (splitStr line ' ')
        headTail (h:t) = (h,t)
    in headTail tokens

findPointWithLabel label (p:ps) = if label == (getLabel p) then p
                                    else findPointWithLabel label ps

printElements :: [[Char]] -> IO()
printElements [] = return ()
printElements (x:xs) = do putStrLn x
                          printElements xs

getLabelMapFromInput :: Map [Char] [Point] -> [Point] -> IO ()
getLabelMapFromInput aMap points = do
    end <- isEOF
    if end then
        printElements 
            $ map (\t -> (show t)++"\n")
            $ map (\(label, pts) -> (label, (map getLabel pts)))
            $ StrictMap.assocs
            $ enrich aMap points
    else do
        line <- getLine
        let (label, labelPts) = (getLineLabelTuple line) in
            getLabelMapFromInput (
                foldl (\aMap -> \pt ->
                    attachLabel aMap label pt
                ) aMap (filter (\pt -> (getLabel pt) `elem` labelPts) points)
            ) (filter (\pt -> (getLabel pt) `notElem` labelPts) points)

getPointsFromInput :: [Point] -> IO ()
getPointsFromInput points = do
    end <- isEOF
    if end then
        print points
    else do
        line <- getLine
        if line == "" then
            getLabelMapFromInput StrictMap.empty points
        else
            getPointsFromInput ((createPointFromLine line):points)

main :: IO ()
main = getPointsFromInput []