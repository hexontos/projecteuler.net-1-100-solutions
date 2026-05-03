import Data.Char (digitToInt)
import qualified Data.IntSet as IS


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


problemSix :: Integer
problemSix = sum [1..100] ^ 2 - sum (map (^2) [1..100])


problemSeven :: Integer
problemSeven = last $ take 10_001 (2 : [n | n <- [3,5..],
    let isPrime x = all (\p -> x `mod` p /= 0) (takeWhile (\p -> p * p <= x) (2 : [k | k <- [3,5..], isPrime k]))
    in isPrime n])


problemEight :: Int
problemEight = maximum $ map (product . map digitToInt) [take 13 $ drop indexes inp | indexes <- [0.. length inp - 13]]
    where
        inp = "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"


-- (euclid formula) a = m² - n², b = 2mn, c = m² + n²
problemNine :: Integer
problemNine = sum [ a * b * c | m <- [2..22], n <- [1..m-1],
        gcd m n == 1,
        odd m /= odd n,
        let base = 2 * m * (m + n),
        1000 `mod` base == 0,
        let k = 1000 `div` base,
        let x = k * (m^2 - n^2),
        let y = k * (2 * m * n),
        let (a, b) = if x < y then (x, y) else (y, x),
        let c = k * (m^2 + n^2),
        a < b && b < c
    ]


problemTen :: Integer
problemTen = sum $ map fromIntegral $ sieve 2 (IS.fromDistinctAscList [2..2_000_000])
    where
        sieve p s
            | p * p > 2_000_000 = IS.elems s
            | otherwise = sieve nextP (IS.filter (\x -> x == p || x `mod` p /= 0) s)
            where
                nextP = head $ dropWhile (\x -> not (x `IS.member` s)) [p+1..]
