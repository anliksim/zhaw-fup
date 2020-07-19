-- | cf.
-- https://gitlab.com/olodnad/fup-zhaw/-/blob/master/Exercises/Sheet2/Serie2.pdf
-- Aufgabe 3

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MagicHash #-}

module Fractran where

import Control.Monad (guard)

import Data.Ratio (numerator, denominator, (%))
import Data.Maybe (mapMaybe, fromMaybe, catMaybes)

import GHC.Integer (integerToWord)
import GHC.Exts (Word(W#))
import Data.Bits (Bits, popCount, shiftR, finiteBitSize, countTrailingZeros)

default (Integer)

main :: IO ()
main = do
  putStrLn $ "Test passed: " ++ (show $ testPrimes && testLog2)
  let n = 10
  putStrLn $ "The first " ++ show n ++ " primes: " ++ (show $ take n primes)

data FProgram = FProgram Integer [Rational]
  deriving (Show, Eq, Ord)

-- Running (n fsRemaining fsAll)
data FState = Running Integer [Rational] [Rational] | Stopped
  deriving (Show, Eq, Ord)

initFState :: FProgram -> FState
initFState (FProgram n fs) = Running n fs fs

tryOnEntry :: Integer -> Rational -> Maybe Integer
tryOnEntry n f =
  if denominator nf == 1
  then Just (numerator nf)
  else Nothing
  where
    nf = fromIntegral n * f

step :: FState -> FState
step Stopped = Stopped
step (Running _n [] _fsAll) = Stopped
step (Running n (f : fs) fsAll) = case n `tryOnEntry` f of
  Just nf -> Running nf fsAll fsAll
  Nothing -> Running n fs fsAll

run :: FProgram -> [Integer]
run p = getRes fStates where
  fStates = iterate step (initFState p)
  getRes [] = [] -- impossible due to iterate
  getRes (Stopped : _ss) = []
  getRes ((Running n fs fsAll) : ss) =
    if fs == fsAll
    then n : rec 
    else rec
    where
      rec = getRes ss

primeGame :: FProgram
primeGame = FProgram 2 [17 % 91, 78 % 85, 19 % 51, 23 % 38, 29 % 33, 77 % 29, 95 % 23, 77 % 19, 1 % 17, 11 % 13, 13 % 11, 15 % 14, 15 % 2, 55 % 1]

primeGameOutput :: [Integer]
primeGameOutput = run primeGame

primes :: [Integer]
primes = tail $ mapMaybe log2 (run primeGame)

testPrimes :: Bool
testPrimes = take 7 primes == [2,3,5,7,11,13,17]

log2 :: Integer -> Maybe Integer
log2 n = do
  guard (popCount n == 1)
  pure $ log2' n where
    log2' m = fromMaybe (wordBitSize + log2' (shiftR m wordBitSize)) (tryLog2 m)
    wordBitSize = fromIntegral $ finiteBitSize (0 :: Word)
    tryLog2 m = do
      guard (zeroCount /= wordBitSize)
      pure zeroCount where
        zeroCount = fromIntegral $ countTrailingZeros $ W# (integerToWord m)

powersOfTwo :: [Integer]
powersOfTwo = (2^) <$> [0..]
log2s :: [Integer]
log2s = catMaybes $ log2 <$> powersOfTwo

testLog2 :: Bool
testLog2 = log2s !! 42 == 42 && log2s !! 80 == 80
