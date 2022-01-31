module Armstrong
    ( armstrongNumbers
    ) where

import Control.Monad ( guard )
import Control.Applicative ( Alternative )
import Math.NumberTheory.Primes.Testing ( millerRabinV )
import Data.WideWord.Int128 ( Int128 )

type Wide = Int128

isPrime :: Integral a => a -> Bool
isPrime = millerRabinV 5 . toInteger

digits :: Integral a => a -> [a]
digits n = let (m, d) = divMod n 10 in d : (guard (m > 0) >> digits m)

getDigitmap :: (Integral a, Num b) => a -> b -> b
getDigitmap n nDigits = sum . map ((nDigits + 1)^) . digits $ n

getSump :: (Integral a, Integral b) => a -> a -> b -> a
getSump digitmap nDigits len = loop (digitmap `div` (nDigits + 1)) 1
    where loop _ 10 = 0
          loop dm d = let (nextDm, dCount) = dm `divMod` (nDigits + 1) 
                      in dCount * d ^ len + loop nextDm (d + 1)

testSump :: (Integral a, Integral b, Monad m, Alternative m) => a -> a -> b -> m a
testSump digitmap nDigits len = guard (isPrime sump && getDigitmap sump nDigits == digitmap) >> return sump
    where sump = getSump digitmap nDigits len

forAllCombos :: (Integral a, Num b, Eq b) => a -> b -> a -> a -> [a]
forAllCombos nDigits 0 digitmap cLen =  testSump digitmap nDigits cLen
forAllCombos nDigits depth digitmap cLen = [0..nDigits-cLen] >>= \i ->
    forAllCombos nDigits (depth - 1) (digitmap * (nDigits + 1) + i) (cLen + i)

armstrongNumbers :: [Wide]
armstrongNumbers = forAllCombos 23 10 0 0
