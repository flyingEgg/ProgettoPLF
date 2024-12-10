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

import Data.List (sortOn)
import Data.Maybe (mapMaybe)
import Data.Ord (Down (..))

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
    putStrLn "Benvenuto al sistema avanzato di raccomandazione di canzoni!"
    putStrLn "Inserire il nome del file contenente l'elenco delle canzoni (es. canzoni.txt):"
    putStrLn "Il file deve essere strutturato come segue (un esempio per riga):"
    putStrLn "Titolo,Artista,Genere,Punteggio"
    putStrLn "Es.: Shape of You,Ed Sheeran,Pop,9"
    nomeFile <- getLine
    contenuto <- readFile nomeFile
    let canzoni = mapMaybe parseCanzone (lines contenuto)
    if null canzoni
        then putStrLn "Errore: il file non contiene dati validi!"
        else do
            putStrLn "Inserire i generi musicali preferiti separati da una virgola (es. Pop,Rock):"
            inputGeneri <- getLine
            let generiPreferiti = map trim (split ',' inputGeneri)
            putStrLn "Quanto peso vuoi dare ai generi preferiti? (Es. 1.5):"
            pesoStr <- getLine
            let peso = read pesoStr :: Double
            let raccomandate = raccomandaCanzoni generiPreferiti peso canzoni
            if null raccomandate
                then putStrLn "Nessuna canzone trovata per i generi specificati."
                else do
                    putStrLn "Ecco le canzoni raccomandate per te:"
                    mapM_ stampaCanzone raccomandate

-- Parsing di una riga dal file di testo in una struttura Canzone.
parseCanzone :: String -> Maybe Canzone
parseCanzone riga =
    case split ',' riga of
        [titolo, artista, genere, punteggioStr]
            | "" `notElem` [titolo, artista, genere, punteggioStr],
                all (`elem` "0123456789") punteggioStr ->
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

-- Funzione per rimuovere spazi bianchi attorno a una stringa.
trim :: String -> String
trim = unwords . words

-- Funzione per raccomandare canzoni in base ai generi e al punteggio ponderato.
raccomandaCanzoni :: [String] -> Double -> [Canzone] -> [Canzone]
raccomandaCanzoni generiPeso peso canzoni =
    let conPeso c = if genere c `elem` generiPeso
                    then fromIntegral (punteggio c) * peso
                    else fromIntegral (punteggio c)
        arricchite = map (\c -> (conPeso c, c)) canzoni
        ordinate = sortOn (Down . fst) arricchite -- Utilizza Data.Ord.Down per ordinare decrescentemente
    in map snd ordinate

-- Funzione per stampare una canzone in modo leggibile.
stampaCanzone :: Canzone -> IO ()
stampaCanzone (Canzone titolo artista genere punteggio) =
    putStrLn $ titolo ++ " - " ++ artista ++ " (" ++ genere ++ "), Punteggio: " ++ show punteggio
