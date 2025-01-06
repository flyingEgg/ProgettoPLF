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

import Data.List (sortOn, nub, intercalate)
import Data.Maybe (mapMaybe)
import Data.Ord (Down(..))
import qualified Data.Map as Map
import System.IO.Error(isDoesNotExistError)
import Control.Exception (catch, IOException)
import Text.Read (readMaybe)

-- #########################################################
-- Definizioni dei tipi di dati
-- #########################################################

-- | La struttura 'Canzone' rappresenta una canzone con:
-- - titolo: il titolo della canzone.
-- - artista: l'artista che la interpreta.
-- - genere: il genere musicale della canzone.
-- - punteggio: un punteggio assegnato (da 1 a 10).
data Canzone = Canzone
    { titolo    :: String
    , artista   :: String
    , genere    :: String
    , punteggio :: Int
    } deriving (Show, Eq)

-- | 'PesiGeneri' è una mappa che associa un genere musicale a un peso
-- che influenza la priorità delle raccomandazioni.
type PesiGeneri = Map.Map String Double

-- #########################################################
-- Main: Menu interattivo
-- #########################################################

-- | Funzione principale che avvia il menu interattivo.
main :: IO ()
main = menuLoop Nothing Map.empty

-- | 'menuLoop' gestisce il menu principale, mantenendo lo stato del sistema:
-- - maybeCanzoni: un elenco opzionale delle canzoni caricate.
-- - pesi: i pesi dei generi preferiti, gestiti dall'utente.
menuLoop :: Maybe [Canzone] -> PesiGeneri -> IO ()
menuLoop maybeCanzoni pesi = do
    putStrLn "\n--- Sistema di Raccomandazione Musicale ---"
    putStrLn "1. Carica un file con le canzoni"
    putStrLn "2. Gestisci i generi preferiti (aggiungi o modifica)"
    putStrLn "3. Stampa la classifica delle canzoni"
    putStrLn "4. Stampa i generi preferiti con il relativo punteggio"
    putStrLn "5. Esci"
    putStrLn "Seleziona un'opzione:"
    scelta <- getLine
    case scelta of
        "1" -> do
            canzoni <- caricaCanzoni
            menuLoop (Just canzoni) pesi
        "2" -> do
            nuoviPesi <- selezionaGeneriPreferitiEImpostaPesi maybeCanzoni pesi
            menuLoop maybeCanzoni nuoviPesi
        "3" -> do
            raccomandaCanzoni maybeCanzoni pesi
            menuLoop maybeCanzoni pesi
        "4" -> do
            visualizzaGeneriPreferiti pesi
            menuLoop maybeCanzoni pesi
        "5" -> putStrLn "Grazie per aver usato il sistema di raccomandazione. Arrivederci!"
        _   -> do
            putStrLn "Opzione non valida. Riprova."
            menuLoop maybeCanzoni pesi

-- #########################################################
-- Funzioni di caricamento e gestione dei dati
-- #########################################################

-- | 'caricaCanzoni' legge un file di testo, carica i dati delle canzoni e li
-- trasforma in una lista di 'Canzone'.
-- Il file deve avere un formato valido: Titolo,Artista,Genere,Punteggio.
caricaCanzoni :: IO [Canzone]
caricaCanzoni = do
    nomeFile <- chiediNomeFile
    contenuto <- readFile nomeFile
    let canzoni = mapMaybe analizzaCanzone (lines contenuto)
    if null canzoni
        then do
            putStrLn "Errore: il file non contiene dati validi! Controllare la formattazione e riprovare."
            caricaCanzoni
        else do
            putStrLn "File caricato con successo!"
            return canzoni

-- | 'chiediNomeFile' richiede all'utente di inserire il nome del file con le canzoni
-- e ne effettua una validazione dell'input tramite la funzione validaFile.
chiediNomeFile :: IO FilePath
chiediNomeFile = do
    putStrLn "Inserire il nome del file"
    nomeFile <- getLine
    esito_lettura <- validaFile nomeFile
    case esito_lettura of
        Right _ -> return nomeFile
        Left err -> do
            putStrLn $ "Errore: " ++ err
            chiediNomeFile

-- | 'validaFile' funge come tramite per controllare se il nome del file e' espresso
-- correttamente e se tale file specificato esista. 
validaFile :: FilePath -> IO (Either String ())
validaFile nomeFile = do
    catch (do
        contenuto <- readFile nomeFile
        length contenuto `seq` return (Right()))
        (\e -> if isDoesNotExistError e
                then return $ Left "File non trovato!"
                else return $ Left "Errore durante l'apertura del file.")

-- | 'selezionaGeneriPreferitiEImpostaPesi' permette all'utente di scegliere
-- i generi preferiti e assegnare un peso a ciascuno di essi.
selezionaGeneriPreferitiEImpostaPesi :: Maybe [Canzone] -> PesiGeneri -> IO PesiGeneri
selezionaGeneriPreferitiEImpostaPesi Nothing pesi = do
    putStrLn "Errore: nessun file caricato. Carica un file prima di continuare."
    return pesi
selezionaGeneriPreferitiEImpostaPesi (Just canzoni) pesi = do
    let generiDisponibili = nub $ map genere canzoni
    putStrLn $ "Generi disponibili: " ++ intercalate ", " generiDisponibili
    generiSelezionati <- raccogliGeneriPreferiti generiDisponibili
    aggiornaPesi generiSelezionati pesi

-- | 'raccogliGeneriPreferiti' consente all'utente di inserire i generi
-- preferiti uno alla volta, terminando con "fine".
raccogliGeneriPreferiti :: [String] -> IO [String]
raccogliGeneriPreferiti generiDisponibili = do
    putStrLn "Inserisci i generi preferiti uno alla volta. Scrivi 'fine' per terminare."
    raccogliGeneri [] generiDisponibili

-- | 'raccogliGeneri' gestisce il processo iterativo di raccolta dei generi.
raccogliGeneri :: [String] -> [String] -> IO [String]
raccogliGeneri acc generiDisponibili = do
    putStrLn "Inserisci un genere preferito:"
    input <- getLine
    if input == "fine"
        then return (nub acc)
        else if input `elem` generiDisponibili
            then do
                putStrLn $ "Genere '" ++ input ++ "' aggiunto ai preferiti."
                raccogliGeneri (input : acc) generiDisponibili
            else do
                putStrLn "Genere non valido. Riprova."
                raccogliGeneri acc generiDisponibili

-- | 'aggiornaPesi' consente all'utente di modificare i pesi dei generi preferiti.
-- Se il genere ha già un peso, l'utente può scegliere di mantenerlo o aggiornarlo.
aggiornaPesi :: [String] -> PesiGeneri -> IO PesiGeneri
aggiornaPesi [] pesi = return pesi
aggiornaPesi (g:gs) pesi = do
    let pesoCorrente = Map.findWithDefault 1.0 g pesi
    putStrLn $ "Peso corrente per il genere '" ++ g ++ "': " ++ show pesoCorrente
    putStrLn "Vuoi aggiornare il peso? (s/n)"
    risposta <- getLine
    if risposta == "s"
        then do
            putStrLn $ "Inserisci il nuovo peso per il genere '" ++ g ++ "':"
            nuovoPeso <- leggiPesoValido
            aggiornaPesi gs (Map.insert g nuovoPeso pesi)
        else do
            putStrLn $ "Peso per il genere '" ++ g ++ "' invariato."
            aggiornaPesi gs pesi

-- | 'validaPeso' richiede all'utente di inserire un valore numerico valido per il peso.
validaPeso :: String -> IO Double
validaPeso genere = do
    putStrLn $ "Inserisci il nuovo peso per il genere '" ++ genere ++ "' (valore positivo):"
    input <- getLine
    case reads input :: [(Double, String)] of
        [(valore, "")] | valore > 0 -> return valore
        _ -> do
            putStrLn "Errore: devi inserire un numero positivo. Riprova."
            validaPeso genere

-- #########################################################
-- Raccomandazioni
-- #########################################################

-- | 'raccomandaCanzoni' genera e stampa una lista di canzoni consigliate
-- basandosi sui pesi dei generi e sui punteggi delle canzoni.
raccomandaCanzoni :: Maybe [Canzone] -> PesiGeneri -> IO ()
raccomandaCanzoni Nothing _ = putStrLn "Errore: nessun file caricato. Carica un file prima di continuare."
raccomandaCanzoni (Just canzoni) pesi = do
    let raccomandate = raccomanda pesi canzoni
    if null raccomandate
        then putStrLn "Nessuna canzone trovata con i pesi attuali."
        else stampaClassifica raccomandate

-- #########################################################
-- Funzioni ausiliarie
-- #########################################################

-- | 'analizzaCanzone' converte una riga di testo in un oggetto 'Canzone'.
-- Restituisce 'Nothing' se la riga non è formattata correttamente.
analizzaCanzone :: String -> Maybe Canzone
analizzaCanzone riga =
    case separaTaglia ',' riga of
        [titolo, artista, genere, punteggioStr]
            | all (/= "") [titolo, artista, genere, punteggioStr]  -- Controlla che tutte le parti siano non vuote
            , Just punteggio <- readMaybe punteggioStr  -- Prova a leggere il punteggio
            , punteggio >= 1 && punteggio <= 10 -> Just (Canzone titolo artista genere punteggio)  -- Verifica che il punteggio sia valido
        _ -> Nothing  -- Restituisce Nothing se la riga non è valida

-- | 'separa' divide una stringa in una lista di stringhe, usando un delimitatore.
separa :: Char -> String -> [String]
separa _ "" = []
separa delimiter string =
            | notElem "" [titolo, artista, genere, punteggioStr] &&
              all (`elem` "0123456789") punteggioStr -> Just (Canzone titolo artista genere (read punteggioStr))
        _ -> Nothing

-- | 'separaTaglia' pulisce gli spazi dai campi separati
separaTaglia :: Char -> String -> [String]
separaTaglia delimiter string = map (filter (/= ' ')) (separa delimiter string)

leggiPesoValido :: IO Double
leggiPesoValido = do
    input <- getLine
    case readMaybe input :: Maybe Double of
            Just peso
                | peso >= 0 && peso <= 10 -> return peso
            Just _ -> do
                putStrLn "Il peso deve essere compreso tra o e 10, riprova."
                leggiPesoValido
            Nothing -> do
                putStrLn "Peso non valido, riprova."
                leggiPesoValido
            

-- | 'raccomanda' calcola il punteggio ponderato per ogni canzone e le ordina.
raccomanda :: PesiGeneri -> [Canzone] -> [(Double, Canzone)]
raccomanda pesi canzoni =
    let arricchite = arricchisci pesi canzoni
    in sortOn (Down . fst) arricchite

-- | 'arricchisci' calcola il punteggio ponderato per ogni canzone.
arricchisci :: PesiGeneri -> [Canzone] -> [(Double, Canzone)]
arricchisci pesi canzoni =
    [ (fromIntegral (punteggio c) * Map.findWithDefault 1.0 (genere c) pesi, c) | c <- canzoni ]

-- | 'stampaClassifica' stampa le canzoni ordinate con il loro punteggio ponderato.
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

-- Effettua una vista dei generi preferiti in memoria
visualizzaGeneriPreferiti :: PesiGeneri -> IO ()
visualizzaGeneriPreferiti pesi
    | Map.null pesi = putStrLn "Nessun genere ancora definito."
    | otherwise = do
        putStrLn "I tuoi generi preferiti e pesi associati sono:"
        mapM_ stampaGenere (Map.toList pesi)

-- Stampa il genere, concatenato al peso suo relativo
stampaGenere :: (String, Double) -> IO ()
stampaGenere (genere, peso) = putStrLn $ genere ++ ": " ++ show peso