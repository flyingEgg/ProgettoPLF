-- #########################################################
-- # Corso di Programmazione Logica e Funzionale           #
-- # Progetto di raccomandazione di canzoni                #
-- # Studente: Giaconi Christian, Giacomo Rossi            #
-- # Matricola: 314045, 314671                             #
-- #########################################################

{- Specifica:
    Scrivere un programma in Haskell per implementare un sistema avanzato di raccomandazione di canzoni.
    Il sistema suggerisce canzoni a un utente in base a:
    - Preferenze per uno o più generi musicali specificati.
    - Un sistema di punteggio ponderato per dare priorità a canzoni più rilevanti.
    L'utente deve fornire un file di testo con le canzoni nel seguente formato:
        Titolo,Artista,Genere,Punteggio
    Dove "Punteggio" è un intero da 1 a 10.
    Le canzoni saranno ordinate in base al punteggio ponderato e filtrate per genere.
-}

module Main where

import Data.List (sortOn)
import Data.Maybe (mapMaybe)
import Data.Ord (Down(..))
import Data.Char (toLower)

-- #########################################################
-- Definizioni dei tipi di dati
-- #########################################################

type GeneriPreferiti = [String]  -- Lista di generi musicali preferiti
type ContenutoFile = String      -- Contenuto del file di input

-- #########################################################
-- # Codice Funzionale                                    #
-- #########################################################

-- Rappresentazione di una canzone
-- Titolo: Nome della canzone
-- Artista: Nome dell'artista
-- Genere: Genere musicale della canzone
-- Punteggio: Gradimento (1-10)
data Canzone = Canzone
    { titolo    :: String
    , artista   :: String
    , genere    :: String
    , punteggio :: Int
    } deriving (Show)

-- #########################################################
-- Funzione main
-- #########################################################

main :: IO ()
main = do
    putStrLn "Benvenuto al sistema avanzato di raccomandazione di canzoni!"
    putStrLn "Inserire il nome del file contenente l'elenco delle canzoni (es. canzoni.txt):"
    putStrLn "Il file deve essere strutturato come segue (un esempio per riga):"
    putStrLn "Titolo,Artista,Genere,Punteggio"
    putStrLn "Es.: Shape of You,Ed Sheeran,Pop,9"
    nomeFile <- getLine
    contenuto <- readFile nomeFile
    let canzoni = mapMaybe analizzaCanzone (righe contenuto)
    if null canzoni
        then putStrLn "Errore: il file non contiene dati validi!"
        else do
            putStrLn "Inserire i generi musicali preferiti separati da una virgola (es. Pop,Rock):"
            inputGeneri <- getLine
            let generiPreferiti = mappaPulisci (dividi ',' inputGeneri)
            putStrLn "Quanto peso vuoi dare ai generi preferiti? (Es. 1.5):"
            pesoStr <- getLine
            let peso = read pesoStr :: Double
            let raccomandate = raccomanda generiPreferiti peso canzoni
            if null raccomandate
                then putStrLn "Nessuna canzone trovata per i generi specificati."
                else do
                    putStrLn "Ecco le canzoni raccomandate per te:"
                    mapM_ stampa raccomandate

-- #########################################################
-- Funzioni di utilità
-- #########################################################

{- Funzione che analizza e trasforma una riga in una struttura Canzone.
    Argomento:
        - riga: Una stringa formattata come "Titolo,Artista,Genere,Punteggio".
    Restituisce:
        - Una struttura Maybe Canzone o Nothing se la riga non è valida.
-}
analizzaCanzone :: String -> Maybe Canzone
analizzaCanzone riga =
    case dividi ',' riga of
        [titolo, artista, genere, punteggioStr]
            | "" `notElem` [titolo, artista, genere, punteggioStr],
                all (`elem` "0123456789") punteggioStr ->
                    Just (Canzone titolo artista genere (read punteggioStr))
        _ -> Nothing

{- Funzione per dividere una stringa in base a un delimitatore usando ricorsione.
    Argomenti:
        - delimitatore: Carattere delimitatore.
        - stringa: Stringa da dividere.
    Restituisce:
        - Lista di sottostringhe separate dal delimitatore.
-}
dividi :: Char -> String -> [String]
dividi _ "" = []
dividi delimitatore stringa =
    let (primo, resto) = break (== delimitatore) stringa
    in primo : dividi delimitatore (drop 1 resto)

{- Funzione per rimuovere spazi bianchi attorno a una stringa.
    Argomento:
        - Una stringa qualsiasi.
    Restituisce:
        - La stringa senza spazi iniziali o finali.
-}
mappaPulisci :: [String] -> [String]
mappaPulisci = map (unwords . words)

{- Funzione che divide un testo in righe usando ricorsione.
    Argomento:
        - testo: Una stringa multilinea.
    Restituisce:
        - Lista di righe.
-}
righe :: String -> [String]
righe "" = []
righe testo =
    let (prima, resto) = break (== '\n') testo
    in prima : righe (drop 1 resto)

-- #########################################################
-- Funzioni principali
-- #########################################################

{- Funzione che raccomanda canzoni basandosi sui generi preferiti e sul punteggio ponderato.
    Argomenti:
        - generiPreferiti: Lista dei generi preferiti.
        - peso: Moltiplicatore del punteggio per i generi preferiti.
        - canzoni: Lista delle canzoni disponibili.
    Restituisce:
        - Lista ordinata di canzoni raccomandate.
-}
raccomanda :: GeneriPreferiti -> Double -> [Canzone] -> [Canzone]
raccomanda generiPreferiti peso canzoni =
    let generiPreferitiMinuscoli = map (map toLower . unwords . words) generiPreferiti  -- Generi in minuscolo
        arricchite = arricchisci generiPreferitiMinuscoli peso canzoni
    in map snd $ sortOn (Down . fst) arricchite  -- Ordina e restituisce solo le canzoni

{- Funzione per arricchire le canzoni con punteggi ponderati usando ricorsione.
    Argomenti:
        - generiPreferiti: Lista dei generi preferiti (in minuscolo).
        - peso: Moltiplicatore per i generi preferiti.
        - canzoni: Lista delle canzoni disponibili.
    Restituisce:
        - Lista di tuple (punteggio ponderato, canzone).
-}
arricchisci :: [String] -> Double -> [Canzone] -> [(Double, Canzone)]
arricchisci _ _ [] = []
arricchisci generiPreferiti peso (c:cs) =
    let genereMinuscolo = map toLower (genere c)
        punteggioPonderato = if genereMinuscolo `elem` generiPreferiti
                             then fromIntegral (punteggio c) * peso
                             else fromIntegral (punteggio c)
    in (punteggioPonderato, c) : arricchisci generiPreferiti peso cs

{- Funzione per stampare una canzone in modo leggibile.
    Argomento:
        - Una struttura Canzone.
    Effetto collaterale:
        - Stampa le informazioni della canzone in modo formattato.
-}
stampa :: Canzone -> IO ()
stampa (Canzone titolo artista genere punteggio) =
    putStrLn $ titolo ++ " - " ++ artista ++ " (" ++ genere ++ "), Punteggio: " ++ show punteggio
