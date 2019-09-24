{-# LANGUAGE Strict #-}
module Main where

-- :set -package containers
import qualified Data.Map.Strict as StrictMap
import Data.List
type Map = StrictMap.Map

sq a = a * a

data Point = Point [Char] [Double]

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

attachLabel :: Point -> [Char] -> Map [Char] [Point] -> Map [Char] [Point]
attachLabel point label mapa
    | StrictMap.member label mapa = StrictMap.adjust (\v -> point:v) label mapa
    | otherwise = StrictMap.insert label [point] mapa

main :: IO ()
--main = print (dist (Point 3.0 4.0) (Point 4.0 3.0))
--main = print $ splitStr "meu amigo feliz" ' '
-- main = print $ dist (Point [3, 4, 5]) (Point [4, 3, 12])
main = print "bla"