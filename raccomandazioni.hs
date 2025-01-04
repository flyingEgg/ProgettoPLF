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

import Data.List (sortOn, nub, sort, intercalate)
import Data.Maybe (mapMaybe)
import Data.Ord (Down(..))
import Data.Char (toLower)
import System.IO.Error(isDoesNotExistError)
import Control.Exception (catch, IOException)

-- #########################################################
-- Definizioni dei tipi di dati
-- #########################################################

{- Struttura dati per rappresentare una canzone.
    Campi:
        - titolo: Nome della canzone.
        - artista: Nome dell'artista.
        - genere: Genere musicale della canzone.
        - punteggio: Gradimento (1-10).
-}
data Canzone = Canzone
    { titolo    :: String
    , artista   :: String
    , genere    :: String
    , punteggio :: Int
    } deriving (Show, Eq)

-- #########################################################
-- Sezione non funzionale
-- #########################################################

main :: IO ()
main = do
    putStrLn "--- Benvenuto nel sistema di raccomandazione musicale! ---"
    putStrLn "Per iniziare, assicurarsi di avere un file con l'elenco delle canzoni."
    putStrLn "Ogni riga del file deve essere strutturata come segue:"
    putStrLn "Titolo,Artista,Genere,Punteggio (dove il punteggio è un intero da 1 a 10)."
    putStrLn "Esempio: Despacito,Luis Fonsi,Reggaeton,9"
    

    nomeFile <- chiediNomeFile
    contenuto <- readFile nomeFile
    
    let canzoni = mapMaybe analizzaCanzone (righe contenuto)
    if null canzoni
        then do
            putStrLn "Errore: il file non contiene dati validi! Controllare la formattazione e riprovare."
            main
        else do
            putStrLn "\n--- Generi musicali disponibili nel file ---"
            let generi = generiDisponibili canzoni
            putStrLn $ "Generi trovati: " ++ intercalate ", " generi
            
            putStrLn "\nInserire uno o più generi preferiti separati da una virgola (es. Salsa,Reggaeton):"
            inputGeneri <- getLine
            let generiPreferiti = mappaPulisci (dividi ',' inputGeneri)
            
            putStrLn "Inserire il peso da assegnare ai generi preferiti (es. 1.5):"
            pesoStr <- getLine
            let peso = read pesoStr :: Double
            
            let raccomandate = raccomanda generiPreferiti peso canzoni
            if null raccomandate
                then putStrLn "Nessuna canzone trovata per i generi specificati. Provare con altri generi."
                else do
                    putStrLn "\n--- Classifica delle canzoni raccomandate ---"
                    putStrLn "Le seguenti canzoni corrispondono alle preferenze specificate, ordinate per punteggio:"
                    stampaClassifica raccomandate

-- #########################################################
-- Sezione funzionale
-- #########################################################


{- Funzione per chiedere e validare il nome del file
    Argomento:
        - Il percorso del file fornito da IO
-}
chiediNomeFile :: IO FilePath
chiediNomeFile = do
    putStrLn "Inserire il nome del file: "
    nomeFile <- getLine
    esito_lettura <- validaFile nomeFile
    case esito_lettura of
        Right _ -> return nomeFile
        Left err -> do
            putStrLn $ "Errore: " ++ err
            chiediNomeFile

{- Funzione per validare l'esistenza e la leggibilita' del file
    Argomento:
        - Il percorso del file
    Restituisce:
        - Una richiesta di input se l'input precedente non e' andato a buon termine
-}
validaFile :: FilePath -> IO (Either String ())
validaFile nomeFile = do
    catch (do
        contenuto <- readFile nomeFile
        length contenuto `seq` return (Right()))
        (\e -> if isDoesNotExistError e
                then return $ Left "File non trovato!"
                else return $ Left "Errore durante l'apertura del file.")

{- Funzione per stampare una classifica di canzoni numerata.
    Argomento:
        - Lista di tuple contenenti il punteggio ponderato e una struttura Canzone.
    Effetto collaterale:
        - Stampa ogni canzone con la sua posizione nella classifica.
-}
stampaClassifica :: [(Double, Canzone)] -> IO ()
stampaClassifica raccomandate =
        mapM_ stampaConPosizione (zip [1..] raccomandate)
    where
        stampaConPosizione (pos, (punteggioPonderato, Canzone titolo artista genere _)) = do
            putStrLn $ "#" ++ show pos ++ " - " ++ titolo
            putStrLn $ "   Artista: " ++ artista
            putStrLn $ "   Genere: " ++ genere
            putStrLn $ "   Punteggio ponderato: " ++ show punteggioPonderato
            putStrLn "-------------------------------------------"

{- Funzione per analizzare una riga del file e trasformarla in una struttura Canzone.
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
                all (`elem` "0123456789") punteggioStr -> Just (Canzone titolo artista genere (read punteggioStr))
        _ -> Nothing

{- Funzione per dividere una stringa in base a un delimitatore.
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

{- Funzione per rimuovere spazi bianchi attorno a una lista di stringhe.
    Argomento:
        - Una lista di stringhe.
    Restituisce:
        - La lista di stringhe senza spazi iniziali o finali.
-}
mappaPulisci :: [String] -> [String]
mappaPulisci = map (unwords . words)

{- Funzione per dividere un testo in righe usando ricorsione.
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

{- Funzione per ottenere una lista di generi musicali unici ordinati alfabeticamente.
    Argomento:
        - Una lista di canzoni.
    Restituisce:
        - Una lista di generi unici in ordine alfabetico.
-}
generiDisponibili :: [Canzone] -> [String]
generiDisponibili = nub . sort . map genere

{- Funzione che calcola la lista ordinata di raccomandazioni basata sui generi preferiti.
    Argomenti:
        - generiPreferiti: Lista dei generi preferiti.
        - peso: Moltiplicatore del punteggio per i generi preferiti.
        - canzoni: Lista delle canzoni disponibili.
    Restituisce:
        - Lista ordinata di tuple (punteggio ponderato, canzone).
-}
raccomanda :: [String] -> Double -> [Canzone] -> [(Double, Canzone)]
raccomanda generiPreferiti peso canzoni =
    let generiPreferitiMinuscoli = map (map toLower . unwords . words) generiPreferiti
        arricchite = arricchisci generiPreferitiMinuscoli peso canzoni
    in sortOn (Down . fst) arricchite

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