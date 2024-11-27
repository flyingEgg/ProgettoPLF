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

-- Definizione della struttura dati per rappresentare una canzone.
data Canzone = Canzone
  { titolo :: String      -- Titolo della canzone
  , artista :: String     -- Nome dell'artista
  , genere :: String      -- Genere musicale
  , punteggio :: Int      -- Punteggio di gradimento (1-10)
  } deriving (Show)

-- Funzione principale per il caricamento e la raccomandazione.
main :: IO ()
main = do
  putStrLn "Benvenuto al sistema di raccomandazione di canzoni!"
  putStrLn "Inserire il file contenente l'elenco delle canzoni (es. canzoni.txt):"
  nomeFile <- getLine
  contenuto <- readFile nomeFile
  let canzoni = map parseCanzone (lines contenuto)
  putStrLn "Inserire il genere musicale preferito:"
  generePreferito <- getLine
  let raccomandate = raccomandaCanzoni generePreferito canzoni
  putStrLn "Ecco le canzoni raccomandate per te:"
  mapM_ stampaCanzone raccomandate

-- Parsing di una riga dal file di testo in una struttura Canzone.
parseCanzone :: String -> Canzone
parseCanzone riga =
  let [titolo, artista, genere, punteggioStr] = split ',' riga
   in Canzone titolo artista genere (read punteggioStr)

-- Funzione per dividere una stringa in base a un delimitatore.
split :: Char -> String -> [String]
split _ "" = []
split delim str =
  let (primo, resto) = break (== delim) str
   in primo : case resto of
        [] -> []
        (_:xs) -> split delim xs

-- Funzione per raccomandare canzoni in base al genere e al punteggio.
raccomandaCanzoni :: String -> [Canzone] -> [Canzone]
raccomandaCanzoni genere canzoni =
  let filtrate = filter (\c -> genere == genere c) canzoni
   in reverse (sortOn punteggio filtrate)

-- Funzione per stampare una canzone in modo leggibile.
stampaCanzone :: Canzone -> IO ()
stampaCanzone (Canzone titolo artista genere punteggio) =
  putStrLn $ titolo ++ " - " ++ artista ++ " (" ++ genere ++ "), Punteggio: " ++ show punteggio
