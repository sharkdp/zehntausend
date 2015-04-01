module Main where

-- Interaktiver Modus

import Data.Function (on)
import Data.List (sortBy)
import Control.Monad
import System.Console.Readline

import Zehntausend

wuerfelZuListe :: [Int] -> [Int]
wuerfelZuListe ws = map count [1 .. 6] ++ [0]
    where count n = length $ filter (==n) ws

strrep :: Int -> String -> String
strrep n str = concat $ replicate n str

listeZuString :: [Int] -> String
listeZuString ns = concat $ zipWith xMalN ns ["1", "2", "3", "4", "5", "6", "1*"]
    where xMalN x n = strrep x (n ++ " ")

erwZeile :: Double -> [Int] -> [Int] -> (Double, String)
erwZeile p ns nstk = (ew, "EW für Kombination " ++ sns ++ spaces ++ ": " ++ show (round ew))
    where sns = listeZuString nstk
          spaces = strrep (13 - length sns) " "
          ew = ewTK p (sum ns) nstk

hilfe :: String
hilfe = "\n\
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
leseBefehl ('h':_) = putStrLn hilfe >> return True
leseBefehl ('i':_) = return $ seq (ew 0 6) True
leseBefehl ('e':' ':rest) =
    let args = words rest
    in if length args == 2 then
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
    in if length args >= 2 then
           let p  = read (head args) :: Double
               ws = map read (tail args) :: [Int]
               ns = wuerfelZuListe ws
               tk = filter verwertbar $ alleTeilKomb ns
               punkteSchreiben = p + fromIntegral (wertung ns)
               paarSchreiben = (punkteSchreiben, "Punkte beim Schreiben           : " ++ show (round punkteSchreiben))
               paare = paarSchreiben : map (erwZeile p ns) tk
               sortPaare = sortBy (compare `on` fst) paare
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

main :: IO ()
main = interaktiverModus
