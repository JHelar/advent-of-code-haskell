import System.IO
import Text.Printf (printf)

toInt :: String -> Int 
toInt x = read x :: Int 

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

readDepths :: IO[Int]
readDepths = do
    contents <- readFile "./day-1/input.txt"
    let depths = map toInt (lines contents)
    return depths

previousReducer :: (Int, Int) -> Int -> (Int , Int)
previousReducer (prev, c) x = (x, if x > prev then c + 1 else c)

window :: Int -> [Int] -> Int
window from xs = sum (slice from (from + 2) xs)

part1 = do
    depths <- readDepths
    let result = snd (foldl previousReducer (head depths + 1, 0) depths)
    printf "Part 1 result: %i\n" result

part2 = do
    depths <- readDepths
    let windows = take (length depths) (map (`window` depths) [0..])
    let result = snd (foldl previousReducer (head windows + 1, 0) windows)
    printf "Part 2 result: %i\n" result