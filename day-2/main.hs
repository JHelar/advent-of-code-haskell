import Data.List
import Text.Printf (printf)
type Command = (String, Int)
type Position = (Int, Int)
type Submarine = (Int, Int, Int)

splitBy :: Char -> String -> [String]
splitBy _ "" = [];
splitBy delimiterChar inputString = foldr f [""] inputString
  where f :: Char -> [String] -> [String]
        f currentChar allStrings@(partialString:handledStrings)
            | currentChar == delimiterChar  = "":allStrings
            | otherwise                     = (currentChar:partialString):handledStrings

toCommand :: String -> Command
toCommand commandString = let x = splitBy ' ' commandString in 
    (head x, read (last x))

readCommands :: IO[Command]
readCommands = do
    contents <- readFile "./day-2/input.txt"
    let commands = map toCommand (lines contents)
    return commands

runPart1 :: Position -> Command -> Position
runPart1 (x, y) (direction, magnitude)
        | direction `isSubsequenceOf` "forward"    = (x + magnitude, y)
        | direction `isSubsequenceOf` "down"       = (x, y + magnitude)
        | direction `isSubsequenceOf` "up"         = (x, y - magnitude)
        | otherwise                                = (x, y)

runPart2 :: Submarine -> Command -> Submarine
runPart2 (x, y, aim) (direction, magnitude)
        | direction `isSubsequenceOf` "forward"    = (x + magnitude, y + aim * magnitude, aim)
        | direction `isSubsequenceOf` "down"       = (x, y, aim + magnitude)
        | direction `isSubsequenceOf` "up"         = (x, y, aim - magnitude)
        | otherwise                                = (x, y, aim)

part1 = do
    commands <- readCommands
    let (x, y) = foldl runPart1 (0, 0) commands
    let result = x * y
    return result

part2 = do
    commands <- readCommands
    let (x, y, _) = foldl runPart2 (0, 0, 0) commands
    let result = x * y
    return result