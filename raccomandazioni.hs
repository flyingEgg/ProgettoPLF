-- #########################################################
-- # Corso di Programmazione Logica e Funzionale           #
-- # Progetto di raccomandazione di canzoni                #
-- # Studente: [Il tuo Nome]                               #
-- # Matricola: [Il tuo Numero]                            #
-- #########################################################

{- Specifica:
    Scrivere un programma Haskell per implementare un sistema di raccomandazione di canzoni.
    Il sistema suggerisce canzoni basandosi sulle preferenze dell'utente (es. genere musicale)
    e utilizza un punteggio di gradimento per ordinare le canzoni più popolari o rilevanti.
-}

import Data.List (sortOn)
import Data.Maybe (mapMaybe)

-- Definizione della struttura dati per rappresentare una canzone.
data Canzone = Canzone
    { titolo    :: String  -- Titolo della canzone
    , artista   :: String  -- Nome dell'artista
    , genere    :: String  -- Genere musicale
    , punteggio :: Int     -- Punteggio di gradimento (1-10)
    } deriving (Show)

-- Funzione principale per il caricamento e la raccomandazione.
main :: IO ()
main = do
    putStrLn "Benvenuto al sistema di raccomandazione di canzoni!"
    putStrLn "Inserire il file contenente l'elenco delle canzoni (es. canzoni.txt):"
    nomeFile <- getLine
    contenuto <- readFile nomeFile
    let canzoni = mapMaybe parseCanzone (lines contenuto)
    if null canzoni
        then putStrLn "Errore: il file non contiene dati validi!"
        else do
        putStrLn "Inserire il genere musicale preferito:"
        genereUtente <- getLine
        let raccomandate = raccomandaCanzoni genereUtente canzoni
        if null raccomandate
            then putStrLn "Nessuna canzone trovata per il genere specificato."
            else do
            putStrLn "Ecco le canzoni raccomandate per te:"
            mapM_ stampaCanzone raccomandate

-- Parsing di una riga dal file di testo in una struttura Canzone.
parseCanzone :: String -> Maybe Canzone
parseCanzone riga =
    case split ',' riga of
        [titolo, artista, genere, punteggioStr]
            | all (/= "") [titolo, artista, genere, punteggioStr] -> 
                Just (Canzone titolo artista genere (read punteggioStr))
        _ -> Nothing

-- Funzione per dividere una stringa in base a un delimitatore.
split :: Char -> String -> [String]
split _ "" = []
split delim str =
    let (primo, resto) = break (== delim) str
        in primo : case resto of
            []     -> []
            (_:xs) -> split delim xs

-- Funzione per raccomandare canzoni in base al genere e al punteggio.
raccomandaCanzoni :: String -> [Canzone] -> [Canzone]
raccomandaCanzoni genereUtente canzoni =
    let filtrate = filter (\c -> genereUtente == genere c) canzoni
        in reverse (sortOn punteggio filtrate)

-- Funzione per stampare una canzone in modo leggibile.
stampaCanzone :: Canzone -> IO ()
stampaCanzone (Canzone titolo artista genere punteggio) =
    putStrLn $ titolo ++ " - " ++ artista ++ " (" ++ genere ++ "), Punteggio: " ++ show punteggio