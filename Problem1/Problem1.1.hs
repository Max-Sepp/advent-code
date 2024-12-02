import System.IO
import Data.List

everyOther :: [a] -> [a]
everyOther [] = []
everyOther (x:xs) = x : everyOther (drop 1 xs)

main :: IO ()
main = do
    inputStr <- readFile "input.txt"
    let input = map read (words inputStr) in
        let left = everyOther input in
            let right = everyOther (tail input) in
                print $ totalDistance left right



totalDistance :: [Int] -> [Int] -> Int
totalDistance xs ys = sum (zipWith (\x y -> abs (x-y)) (sort xs) (sort ys))