import Data.Set (Set)
import qualified Data.Set as Set
data Coord = Coord Int Int deriving (Eq, Ord, Show)

data Direction = U | L | R | D deriving (Show)

data Guard = Guard Coord Direction deriving (Show)

main :: IO ()
main = do
    inputStr <- readFile "input.txt"
    let (obstacles, guard, bounds) = parseInput inputStr
    print $ solve bounds obstacles guard


parseInput :: String -> (Set Coord, Guard, (Int, Int))
parseInput input = (Set.fromList obstacles, guard, (xMax, yMax))
    where
        board = lines input

        yMax = length board
        xMax = length (head board)

        indexedBoard = zip [1..] (map (zip [1..]) board)
        boardWithCoord :: [(Coord, Char)]
        boardWithCoord = 
            concatMap (\(y, bs) -> map (addCoord y) bs) indexedBoard

        obstacles = 
            map (\(coord, _) -> coord) 
                (filter (\(_, c) -> c == '#') boardWithCoord)

        guard = 
            toGuard 
                (head (filter (\(_, c) -> c /= '#' && c /= '.') boardWithCoord))

addCoord :: Int -> (Int, Char) -> (Coord, Char)
addCoord y (x, c) = (Coord x y, c)

toGuard :: (Coord, Char) -> Guard
toGuard (coord, '^') = Guard coord U 
toGuard (coord, '>') = Guard coord R 
toGuard (coord, 'v') = Guard coord D 
toGuard (coord, '<') = Guard coord L

solve :: (Int, Int) -> Set Coord -> Guard -> Int
solve bounds obstacles guard = go guard Set.empty
    where
        checkInBounds = guardInArea bounds
        go :: Guard -> Set Coord -> Int
        go guard@(Guard coord _) visited
            | checkInBounds guard = 
                go (nextPosition obstacles guard) (Set.insert coord visited)
            | otherwise = Set.size visited

guardInArea :: (Int, Int) -> Guard -> Bool
guardInArea (xMax, yMax) (Guard (Coord x y) _) = 
    0 < x && x <= xMax && 0 < y && y <= yMax

nextPosition :: Set Coord -> Guard -> Guard
nextPosition obstacles (Guard originalCoord@(Coord x y) U)
    | Set.member nextCoord obstacles = Guard originalCoord R 
    | otherwise                      = Guard nextCoord U
    where
        nextCoord = Coord x (y - 1)

nextPosition obstacles (Guard originalCoord@(Coord x y) R)
    | Set.member nextCoord obstacles = Guard originalCoord D 
    | otherwise                      = Guard nextCoord R
    where
        nextCoord = Coord (x + 1) y

nextPosition obstacles (Guard originalCoord@(Coord x y) D)
    | Set.member nextCoord obstacles = Guard originalCoord L
    | otherwise                      = Guard nextCoord D
    where
        nextCoord = Coord x (y + 1)

nextPosition obstacles (Guard originalCoord@(Coord x y) L)
    | Set.member nextCoord obstacles = Guard originalCoord U
    | otherwise                      = Guard nextCoord L
    where
        nextCoord = Coord (x - 1) y