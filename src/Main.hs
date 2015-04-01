{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

import Data.List
import Data.MemoTrie
import Control.Monad

import System.Console.Readline

------ Code für Memoization
------ See: http://stackoverflow.com/questions/2217289/haskell-caching-results-of-a-function/2217374#2217374

import Data.Binary
import qualified Data.ByteString.Lazy as BS

mangle :: Double -> [Int]
mangle = map fromIntegral . BS.unpack . encode

unmangle :: [Int] -> Double
unmangle = decode . BS.pack . map fromIntegral

instance HasTrie Double where
    data Double :->: a = DoubleTrie ([Int] :->: a)
    trie f = DoubleTrie $ trie $ f . unmangle
    untrie (DoubleTrie t) = untrie t . mangle

------



-- Würfe sind repräsentiert durch [# Einsen, # Zweier, .., # Sechser, # in Einer umgewandelte Fünfer]

einzelwert :: Int -> Int
einzelwert 1 = 1000
einzelwert n = n * 100

wert :: (Int, Int) -> Int
wert (_, 6)             = 10000
wert (1, a) | a < 3     = a * 100
wert (5, a) | a < 3     = a * 50
wert (n, a) | a >= 3    = (einzelwert n) * 2^(a-3)
wert (_, _)             = 0

wertung :: [Int] -> Int
wertung [1, 1, 1, 1, 1, 1, 0] = 2000
wertung l = (sum (zipWith (curry wert) [1..6] l)) + (100 * (l !! 6))

-- Selektiert Kombinationen die verwertbare Teilkombinationen enthalten
wertvoll :: [[Int]] -> [[Int]]
wertvoll = filter (\k -> wertung k > 0)

alleKomb anz = [[a,b,c,d,e,f,0] | a <- r, b <- r, c <- r, d <- r, e <- r, f <- r, a+b+c+d+e+f == anz]
    where r = [0..6]

komb = wertvoll . alleKomb

alleTeilKomb :: [Int] -> [[Int]]
alleTeilKomb [g,h,i,j,k,l,y] | k >= 2 = nub $ (alleTeilKomb [g,h,i,j,k-2,l,y+1]) ++ (menge g h i j k l y)
alleTeilKomb [g,h,i,j,k,l,z] = menge g h i j k l z

menge g h i j k l z = [[a,b,c,d,e,f,y] | a <- [0..g], b <- [0..h], c <- [0..i], d <- [0..j], e <- [0..k], f <- [0..l], y <- [0..z], a+b+c+d+e+f+y >= 1]

verwertbar :: [Int] -> Bool
verwertbar [1, 1, 1, 1, 1, 1, 0] = True
verwertbar [_, b, c, d, _, f, _] = all zw [b, c, d, f]
    where zw a | (a == 0 || a >= 3) = True
               | otherwise = False

fac x = product [2..x]
haeufigkeit komb = (fromIntegral (fac $ sum komb)) / (fromIntegral (product $ map fac komb))
wahrscheinlichkeit :: [Int] -> Double
wahrscheinlichkeit komb = (haeufigkeit komb) / 6^(fromIntegral (sum komb))

ew :: Double -> Int -> Double
ew = memo2 ew'

-- Entsricht in etwa der Funktion E(p, n)
ew' :: Double -> Int -> Double
ew' punkte 0    | punkte >= maxPunkte = punkte
                | otherwise =           ew punkte 6
ew' punkte anz = sum $ map (ewK punkte) (komb anz)

-- Entspricht in etwa der Funktion E_2(p, w)
ewK :: Double -> [Int] -> Double
ewK punkte k = p * (maximum ((punkte + w) : (map (ewTK punkte anz) ltk)))
    where p = wahrscheinlichkeit k
          w = fromIntegral $ wertung k
          ltk = filter verwertbar $ alleTeilKomb k
          anz = sum k

-- Entspricht der Funktion E_2b(p, w, u)
ewTK :: Double -> Int -> [Int] -> Double
ewTK punkte anz tk = ew (punkte + wtk) (anz - (sum tk))
    where wtk = fromIntegral $ wertung tk

-- Für andere Berechnungen:
ewEinzelwurf anz = sum $ map (\k -> (wahrscheinlichkeit k) * (fromIntegral (wertung k))) (komb anz)

maxPunkte :: Double
maxPunkte = 11900


------ I/O stuff


--- Interaktiver Modus

wuerfelZuListe :: [Int] -> [Int]
wuerfelZuListe ws = (map count [1..6]) ++ [0]
    where count n = length $ filter (==n) ws

strrep n str = concat $ replicate n str

listeZuString :: [Int] -> String
listeZuString ns = concat $ zipWith xMalN ns ["1", "2", "3", "4", "5", "6", "1*"]
    where xMalN x n = strrep x (n ++ " ")

erwZeile :: Double -> [Int] -> [Int] -> (Double, String)
erwZeile p ns nstk = (ew, "EW für Kombination " ++ sns ++ spaces ++ ": " ++ (show (round ew)))
    where sns = listeZuString nstk
          spaces = strrep (13 - (length sns)) " "
          ew = ewTK p (sum ns) nstk

hilfeString = "\n\
        \h(elp):                     Hilfe anzeigen\n\
        \q(uit):                     Beenden\n\
        \i(nit):                     Initialisiere Programm\n\
        \e [p Punkte] [n Würfel]:    Zeige Erwartungswert an Punkten beim\n\
        \                            Weiterwürfeln mit n Würfeln\n\
        \z [p Punkte] w1 (w2 ..):    Zugempfehlung wenn p Punkte erreicht\n\
        \                            sind und w1 ... wn gewürfelt wurde.\n\
        \\n\
        \Zwei Beispiele:\n\
        \(1) Der Spieler hat bereits 800 Punkte und noch 4 Würfel zur\n\
        \    Verfügung. Er will wissen, ob er weiterwürfeln soll.\n\
        \\n\
        \    Er gibt ein: 'e 800 4'\n\
        \    Er bekommt die Antwort 833, d.h. sein Erwartungswert beim\n\
        \    Weiterwürfeln ist höher als 800. Er würfelt weiter.\n\
        \\n\
        \\n\
        \(2) Der Spieler hat beim ersten mal Würfeln in diesem Zug bereits\n\
        \    mit dem Herauslegen einer Eins 100 Punkte erreicht. Jetzt hat\n\
        \    er 1,5,5,2,4 gewürfelt und möchte wissen, wie er weiterspielen\n\
        \    soll.\n\
        \\n\
        \    Er gibt ein: 'z 100 1 5 5 2 4'\n\
        \    Als Ausgabe bekommt er eine Liste der verschiedenen Möglich-\n\
        \    keiten in dieser Situation weiterzuspielen. Beim Schreiben\n\
        \    würde er 300 Punkte bekommen. Am besten ist es jedoch, nur eine\n\
        \    Eins herauszulegen und mit vier Würfeln weiterzuspielen. In\n\
        \    diesem Fall beträgt sein Punkte-Erwartungswert 344.\n"

leseBefehl :: String -> IO Bool
leseBefehl ('q':_) = return False
leseBefehl ('h':_) = putStrLn hilfeString >> return True
leseBefehl ('i':_) = return $ seq (ew 0 6) True
leseBefehl ('e':' ':rest) =
    let args = words rest
    in if (length args == 2) then
           let p = read (head args) :: Double
               n = read (args !! 1) :: Int
           in do
               print (ew p n)
               putStrLn ""
               return True
       else do
           putStrLn "Syntax: ew <p> <n>"
           return True
leseBefehl ('z':' ':rest) =
    let args = words rest
    in if (length args >= 2) then
           let p  = read (head args) :: Double
               ws = map read (tail args) :: [Int]
               ns = wuerfelZuListe ws
               tk = filter verwertbar $ alleTeilKomb ns
               punkteSchreiben = p + fromIntegral (wertung ns)
               paarSchreiben = (punkteSchreiben, "Punkte beim Schreiben           : " ++ (show (round punkteSchreiben)))
               paare = paarSchreiben : (map (erwZeile p ns) tk)
               sortPaare = sortBy (\x y -> compare (fst x) (fst y)) paare
               zeilen = map snd sortPaare
           in do
               mapM_ putStrLn zeilen
               putStrLn ""
               return True
       else do
           putStrLn "Syntax: z <p> <w1> (<w2> .. <wn>)"
           return True
leseBefehl "" = return True
leseBefehl str = do
    putStrLn $ "Unerkannter Befehl '" ++ str ++ "'"
    return True


leseBefehle :: IO ()
leseBefehle = do
    maybeLine <- readline "===> "
    case maybeLine of
        Nothing        -> putStrLn ""
        Just befehlstr -> do addHistory befehlstr
                             weiter <- leseBefehl befehlstr
                             when weiter leseBefehle

interaktiverModus :: IO ()
interaktiverModus = do
    putStrLn "Interaktiver Modus für Zehntausend"
    putStrLn "(Gebe 'h' ein für eine Übersicht aller Befehle)"
    putStrLn ""
    leseBefehle


--- Analyse-Ausgaben
genLine n = "S[" ++ (show n) ++ "] = [" ++ (intercalate ", " (map (show . ew n) [1..6]) ++ "]")
printPythonLines = mapM (putStrLn . genLine) [0, 50..20000]

genLine' n = (show n) ++ " " ++ (unwords (map (show . ew n) [1..6]))
printGnuplotLines = mapM (putStrLn . genLine') [0, 50..20000]

main = interaktiverModus
    --putStrLn $ show $ ewEinzelwurf 6
    --putStrLn $ show $ sum $ map wahrscheinlichkeit $ komb 6
    --putStrLn $ genLine 0
    --putStrLn $ show $ ew maxPunkte 6
    --putStrLn $ show $ ew 11526.7 6
    --printGnuplotLines
    --printPythonLines


