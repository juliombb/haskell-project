{-# LANGUAGE Strict #-}
module Main where

-- :set -package containers
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
attachLabel mapa label point
    | StrictMap.member label mapa = StrictMap.adjust (\v -> point:v) label mapa
    | otherwise = StrictMap.insert label [point] mapa


getDist :: ([Char], [Point], Double, Point) -> Double
getDist (_, _, d, _) = d

simplify :: ([Char], [Point], Double, Point) -> ([Char], Point)
simplify (lbl, _, _, p) = (lbl, p)

minimumDistance :: [Point] -> Point -> Double
minimumDistance [] _ = 0
minimumDistance points p = dist (minimumBy (\p1 -> \p2 -> compare (dist p p1) (dist p p2)) points) p

minimumTuple :: [([Char], [Point])] -> Point -> ([Char], [Point], Double)
minimumTuple pointTuples p =
    minimumBy (\(_, _, d1) -> \(_, _, d2) -> compare d1 d2) pointTupleDistances
    where pointTupleDistances = map (\(lbl, ps1) -> (lbl, ps1, (minimumDistance ps1 p))) pointTuples

getNearestLabel :: Map [Char] [Point] -> Point -> ([Char], [Point], Double)
getNearestLabel mapa p = minimumTuple (StrictMap.assocs mapa) p

getNearestPointAndLabel :: Map [Char] [Point] -> [Point] -> ([Char], Point)
getNearestPointAndLabel mapa ps =
    simplify $ minimumBy (\p1 -> \p2 -> compare (getDist p1) (getDist p2)) distances
    where distances = map (\((a,b,c),d) -> (a,b,c,d)) $ map (\p -> ((getNearestLabel mapa p),p)) ps

enrich :: Map [Char] [Point] -> [Point] -> Map [Char] [Point]
enrich mapa [] = mapa
enrich mapa pontos =
    let (lbl, ponto) = (getNearestPointAndLabel mapa pontos) in
        enrich (attachLabel mapa lbl ponto) (delete ponto pontos)

getLineLabelTuple :: [Char] -> ([Char], [[Char]])
getLineLabelTuple linha =
    let tokens = (splitStr linha ' ')
        headTail (h:t) = (h,t)

        in headTail tokens

findPointWithLabel lbl (p:ps) = if lbl == (getLabel p) then p else findPointWithLabel lbl ps

printElements :: [[Char]] -> IO()
printElements [] = return ()
printElements (x:xs) = do putStrLn x
                          printElements xs

getLabelMapFromInput :: Map [Char] [Point] -> [Point] -> IO ()
getLabelMapFromInput mapa pontos = do
    end <- isEOF
    if end then
        printElements 
            $ map (\t -> (show t)++"\n")
            $ map (\(lbl, pts) -> (lbl, (map getLabel pts)))
            $ StrictMap.assocs
            $ enrich mapa pontos
    else do
        linha <- getLine
        let (lbl, lblPts) = (getLineLabelTuple linha) in
            getLabelMapFromInput (
                foldl (\mapa -> \pt ->
                    attachLabel mapa lbl pt
                ) mapa (filter (\pt -> (getLabel pt) `elem` lblPts) pontos)
            ) (filter (\pt -> (getLabel pt) `notElem` lblPts) pontos)

getPointsFromInput :: [Point] -> IO ()
getPointsFromInput pontos = do
    end <- isEOF
    if end then
        print pontos
    else do
        linha <- getLine
        if linha == "" then getLabelMapFromInput StrictMap.empty pontos
        else getPointsFromInput ((createPointFromLine linha):pontos)

main :: IO ()
--main = print (dist (Point 3.0 4.0) (Point 4.0 3.0))
--main = print $ splitStr "meu amigo feliz" ' '
-- main = print $ dist (Point [3, 4, 5]) (Point [4, 3, 12])
main = getPointsFromInput []