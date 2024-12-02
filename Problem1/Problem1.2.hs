import System.IO
import Data.List
import qualified Data.Map as Map
import Data.Maybe

everyOther :: [a] -> [a]
everyOther [] = []
everyOther (x:xs) = x : everyOther (drop 1 xs)

main :: IO ()
main = do
    inputStr <- readFile "input.txt"
    let input = map read (words inputStr) in
        let left = everyOther input in
            let right = everyOther (tail input) in
                print $ similarityScore left right

similarityScore :: [Int] -> [Int] -> Int
similarityScore xs ys = sum (map (\x -> x * (getElement x rightMap)) xs)
    where
        rightMap = foldr insertOrIncrement Map.empty ys

getElement :: Int -> Map.Map Int Int -> Int
getElement x map = maybe 0 id (Map.lookup x map)

insertOrIncrement :: Int -> Map.Map Int Int -> Map.Map Int Int
insertOrIncrement x map 
    | Map.member x map = Map.insertWith (+) x 1 map
    | otherwise = Map.insert x 1 map