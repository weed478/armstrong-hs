module Armstrong
    ( armstrongNumbers
    ) where

import Control.Monad ( guard, MonadPlus )
import Math.NumberTheory.Primes.Testing ( millerRabinV )

isPrime :: Integer -> Bool
isPrime = millerRabinV 5

digits :: Integral a => a -> [a]
digits n = let (m, d) = n `quotRem` 10 in d : (guard (m > 0) >> digits m)

getDigitmap :: (Num a, Integral b) => a -> b -> a
getDigitmap radix = sum . map (radix ^) . digits

getSump :: (Integral a, Integral b) => a -> b -> a -> a
getSump radix len digitmap = fst $ foldl loop (0, digitmap `quot` radix) [1..9]
    where loop (sump, dm) d = let (nextDm, dCount) = dm `quotRem` radix
                              in (sump + dCount * d ^ len, nextDm)

getArmstrong :: (Integral a, MonadPlus m) => Integer -> a -> Integer -> m Integer
getArmstrong radix len digitmap = guard isArmstrong >> return sump
    where sump = getSump radix len digitmap
          isArmstrong = isPrime sump && getDigitmap radix sump == digitmap

forAllCombos :: Integer -> Integer -> Integer -> Int -> [Integer]
forAllCombos radix len digitmap 0 = getArmstrong radix len digitmap
forAllCombos radix len digitmap depth = [0..radix-len-1] >>= \i ->
    forAllCombos radix (len + i) (digitmap * radix + i) (depth - 1)

armstrongNumbers :: [Integer]
armstrongNumbers = take 7 $ forAllCombos 24 0 0 10
