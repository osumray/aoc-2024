import Data.List (sort)

totalDistance :: [Int] -> [Int] -> Int
totalDistance xs ys = sum $ zipWith dist (sort xs) (sort ys)
    where dist x y = abs (x - y)

parseInput :: String -> ([Int], [Int])
parseInput s = (left, right)
    where 
        asInts = map read . words
        paired = map asInts . lines $ s
        left = map head paired 
        right =  map last paired

firstStar :: [Int] -> [Int] -> Int
firstStar = totalDistance
    
testInput :: String
testInput = "3   4\n4   3\n2   5\n1   3\n3   9\n3   3\n"

count :: (Eq a) => a -> [a] -> Int
count x = length . filter (==x)

similarityScore :: [Int] -> [Int] -> Int
similarityScore left right = let 
    counts = traverse count left right
    scores = zipWith (*) left counts 
    in sum scores 

secondStar :: [Int] -> [Int] -> Int
secondStar = similarityScore

main :: IO ()
main = do
    (left, right) <- parseInput <$> readFile "../data/day1.txt"
    print $ firstStar left right
    print $ secondStar left right