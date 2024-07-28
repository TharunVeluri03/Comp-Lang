import Data.List (nub)

-- Function to check if a number is prime
isPrime :: Int -> Bool
isPrime n
    | n <= 1 = False
    | n == 2 = True
    | even n = False
    | otherwise = not (any (divides n) [3,5..limit])
    where
        divides num x = num `mod` x == 0
        limit = floor (sqrt (fromIntegral n) :: Double)

-- Function to generate all possible combinations of three-digit numbers with no repeated digits
generator1 :: [(Int, Int, Int)]
generator1 = [(p1, p2, p3) |
              p1 <- threeDigitPrimes, 
              p2 <- threeDigitPrimes, 
              p3 <- threeDigitPrimes,
              let digits = numToDigits p1 ++ numToDigits p2 ++ numToDigits p3,
              length (nub digits) == 9,
              p1 < p2, p2 < p3]  -- Ensure ascending order

-- Generate all three-digit primes with unique digits
threeDigitPrimes :: [Int]
threeDigitPrimes = filter (\n -> length (nub (numToDigits n)) == 3) $ filter isPrime [100..999]

-- Function to test if a tuple of three-digit primes meets the criteria
tester1 :: (Int, Int, Int) -> Bool
tester1 (n1, n2, n3) =
  let digitSumOdd n = odd $ sum $ numToDigits n
  in isPrime n1 && isPrime n2 && isPrime n3 &&
     digitSumOdd n1 && digitSumOdd n2 && digitSumOdd n3 &&
     (n2 - n1 == n3 - n2)

-- Helper function to convert a number to a list of digits
numToDigits :: Int -> [Int]
numToDigits 0 = []
numToDigits n = numToDigits (n `div` 10) ++ [n `mod` 10]

-- Main function to find the answer using generator1 and tester1
main :: IO ()
main = print $ filter tester1 generator1





