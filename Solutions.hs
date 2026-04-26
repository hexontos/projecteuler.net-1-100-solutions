problemOne :: Integer
problemOne = sum [x | x <- [1..999], x `mod` 5 == 0 || x `mod` 3 == 0]


problemTwo :: Integer
problemTwo = (sum . filter even) $ takeWhile (< 4_000_000) fibs
    where fibs = 1 : 2 : zipWith (+) fibs (drop 1 fibs)


problemThree :: Integer
problemThree = largestPrimeFactor 600851475143 2
    where
        largestPrimeFactor :: Integer -> Integer -> Integer
        largestPrimeFactor n d
            | d^2 > n = n
            | n `mod` d == 0 = largestPrimeFactor (n `div` d) 2
            | otherwise = largestPrimeFactor n (d + 1)


problemFour :: Integer
problemFour = foldr1 (\x y -> if x >= y then x else y) [x * y | x <- [100..999], y <- [100..999], isPalindrome (x * y)]
    where
        isPalindrome :: Integer -> Bool
        isPalindrome n = show n == reverse (show n)

-- without using strings
problemFour' :: Integer
problemFour' = maximum [x * y | x <- [100..999], y <- [100..999], isPalindrome (x * y)]
    where
        isPalindrome :: Integer -> Bool
        isPalindrome n = n == reverseNum n

        reverseNum :: Integer -> Integer
        reverseNum 0 = 0
        reverseNum n = (n `mod` 10) * 10^(digits n - 1) + reverseNum (n `div` 10)

        digits :: Integer -> Integer
        digits n
            | n < 10    = 1
            | otherwise = 1 + digits (n `div` 10)


problemFive :: Integer
problemFive = smallestDivisible 2 20 20
    where
        smallestDivisible :: Integer -> Integer -> Integer -> Integer
        smallestDivisible _ n 1 = n
        smallestDivisible step n d
            | n `mod` d == 0 = smallestDivisible step n $ (-) d 1
            | otherwise = smallestDivisible (step + 1) (20 * step) 20

problemFive' :: Integer
problemFive' = foldl lcm 1 [1..20]