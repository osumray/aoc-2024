type Report = [Int]

type Input = [Report]

testInputString = "7 6 4 2 1\n1 2 7 8 9\n9 7 6 2 1\n1 3 2 4 5\n8 6 4 4 1\n1 3 6 7 9"

parseInput :: String -> Input
parseInput = map (map read . words) . lines

difference :: [Int] -> [Int]
difference xs = zipWith (-) (drop 1 xs) xs

isSMonotonicDifference :: [Int] -> Bool
isSMonotonicDifference = isConstant . map signum
    where isConstant xs = and $ zipWith (==) (drop 1 xs) xs

isGradualDifference :: [Int] -> Bool
isGradualDifference = all (((&&) <$> (>=1) <*> (<=3)) . abs)

isSafe :: Report -> Bool
isSafe = and . sequenceA [isSMonotonicDifference, isGradualDifference] . difference

testDampened :: (Report -> Bool) -> Report -> Bool 
testDampened test r = helper (length r) test r 
    where 
        helper (-1) test r = False
        helper n test r = test (remove n r) || helper (n - 1) test r
        remove n l = take n l ++ drop (n + 1) l

isAlmostSafe :: Report -> Bool 
isAlmostSafe = testDampened isSafe

main :: IO ()
main = do 
    input <- parseInput <$> readFile "../data/day2.txt"
    putStrLn "First Star"
    print . sum . map (fromEnum . isSafe) $ input
    putStrLn "Second Star"
    print . sum . map (fromEnum . isAlmostSafe) $ input