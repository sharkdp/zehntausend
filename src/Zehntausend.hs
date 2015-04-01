module Zehntausend (
      ewTK
    , ew
    , verwertbar
    , alleTeilKomb
    , wertung
    ) where

import Data.List
import Data.MemoTrie (memo2)
import Memoization

-- Würfe sind repräsentiert durch eine Liste mit:
-- [# Einsen, # Zweier, .., # Sechser, # in Einer umgewandelte Fünfer]

einzelwert :: Int -> Int
einzelwert 1 = 1000
einzelwert n = n * 100

wert :: (Int, Int) -> Int
wert (_, 6)             = 10000
wert (1, a) | a < 3     = a * 100
wert (5, a) | a < 3     = a * 50
wert (n, a) | a >= 3    = 2^(a-3) * einzelwert n
wert (_, _)             = 0

wertung :: [Int] -> Int
wertung [1, 1, 1, 1, 1, 1, 0] = 2000
wertung l = sum (zipWith (curry wert) [1..6] l) + 100 * (l !! 6)

-- Selektiert Kombinationen die verwertbare Teilkombinationen enthalten
wertvoll :: [[Int]] -> [[Int]]
wertvoll = filter (\k -> wertung k > 0)

alleKomb :: Int -> [[Int]]
alleKomb anz = [[a, b, c, d, e, f, 0] | a <- r, b <- r, c <- r,
                                        d <- r, e <- r, f <- r,
                                        a + b + c + d + e + f == anz]
    where r = [0 .. 6]

komb :: Int -> [[Int]]
komb = wertvoll . alleKomb

alleTeilKomb :: [Int] -> [[Int]]
alleTeilKomb [g, h, i, j, k, l, y] | k >= 2 = nub $ alleTeilKomb [g, h, i, j, k - 2, l, y + 1] ++ menge g h i j k l y
alleTeilKomb [g, h, i, j, k, l, z] = menge g h i j k l z

menge g h i j k l z = [[a, b, c, d, e, f, y] | a <- [0 .. g], b <- [0 .. h], c <- [0 .. i],
                                               d <- [0 .. j], e <- [0 .. k], f <- [0 .. l],
                                               y <- [0..z], a + b + c + d + e + f + y >= 1]

verwertbar :: [Int] -> Bool
verwertbar [1, 1, 1, 1, 1, 1, 0] = True
verwertbar [_, b, c, d, _, f, _] = all zw [b, c, d, f]
    where zw a | a == 0 || a >= 3  = True
               | otherwise         = False

fac :: Int -> Int
fac x = product [2..x]

haeufigkeit :: [Int] -> Double
haeufigkeit komb = fromIntegral (fac $ sum komb) / fromIntegral (product $ map fac komb)

wahrscheinlichkeit :: [Int] -> Double
wahrscheinlichkeit komb = haeufigkeit komb / 6^(fromIntegral (sum komb))

-- ew ist die 'memoized' version von ew'
ew :: Double -> Int -> Double
ew = memo2 ew'

-- Entsricht in etwa der Funktion E(p, n)
ew' :: Double -> Int -> Double
ew' punkte 0    | punkte >= maxPunkte = punkte
                | otherwise =           ew punkte 6
ew' punkte anz = sum $ map (ewK punkte) (komb anz)

-- Entspricht in etwa der Funktion E_2(p, w)
ewK :: Double -> [Int] -> Double
ewK punkte k = p * maximum (punkte + w : map (ewTK punkte anz) ltk)
    where p = wahrscheinlichkeit k
          w = fromIntegral $ wertung k
          ltk = filter verwertbar $ alleTeilKomb k
          anz = sum k

-- Entspricht der Funktion E_2b(p, w, u)
ewTK :: Double -> Int -> [Int] -> Double
ewTK punkte anz tk = ew (punkte + wtk) (anz - sum tk)
    where wtk = fromIntegral $ wertung tk

-- Für andere Berechnungen:
ewEinzelwurf :: Int -> Double
ewEinzelwurf anz = sum $ map (\k -> wahrscheinlichkeit k * fromIntegral (wertung k)) (komb anz)

maxPunkte :: Double
maxPunkte = 11900
