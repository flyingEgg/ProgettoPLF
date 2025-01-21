-- #########################################################
-- # Corso di Programmazione Logica e Funzionale           #
-- # Progetto di raccomandazione di canzoni                #
-- # Studente: Giaconi Christian, Giacomo Rossi            #
-- # Matricola: 314045, 314671                             #
-- #########################################################

{-  Specifica: scrivere un programma Haskell per implementare un sistema di raccoandazione di canzoni.
    Il programma legge le canzoni espresse in quattro attributi: titolo, artista,
    genere musicale e punteggio; da un file con valori separati da virgole, il cui nome viene acquisito
    da tastiera e le suggerisce all’utente basandosi sulle sue preferenze musicali, utilizzando un
    punteggio di gradimento acquisito da tastiera e configurato su uno o più generi, per creare una
    classifica con le canzoni ordinate secondo il punteggio ponderato da quello di gradimento del
    o dei generi musicali.
-}

module Main where

-- Caricamento delle funzioni necessarie per le operazioni
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

{- 
   La struttura 'Canzone' rappresenta una canzone con:
   - titolo: il titolo della canzone;
   - artista: l'artista che la interpreta;
   - genere: il genere musicale della canzone;
   - punteggio: un punteggio assegnato (da 1 a 10).
-} 
data Canzone = Canzone
    { titolo    :: String
    , artista   :: String
    , genere    :: String
    , punteggio :: Int
    } deriving (Show, Eq)

{- 
   PesiGeneri è una mappa che associa un genere musicale a un peso
   che influenza la priorità delle raccomandazioni. 
-}
type PesiGeneri = Map.Map String Double

-- #########################################################
-- Menu interattivo
-- #########################################################

{- Azione principale che avvia il menu interattivo. -}
main :: IO ()
main = menuLoop Nothing Map.empty

{- 
   Funzione che gestisce il menu principale, mantenendo lo stato del sistema:
   - maybeCanzoni: un elenco opzionale delle canzoni caricate.
   - pesi: i pesi dei generi preferiti, gestiti dall'utente.
-}
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
        "1" -> caricaCanzoni >>= (`menuLoop` pesi) . Just
        "2" -> selezionaGeneriPreferitiEImpostaPesi maybeCanzoni pesi >>= menuLoop maybeCanzoni
        "3" -> raccomandaCanzoni maybeCanzoni pesi >> menuLoop maybeCanzoni pesi
        "4" -> visualizzaGeneriPreferiti pesi >> menuLoop maybeCanzoni pesi
        "5" -> putStrLn "Grazie per aver usato il sistema di raccomandazione. Arrivederci!"
        _   -> putStrLn "Opzione non valida. Riprova." >> menuLoop maybeCanzoni pesi

-- #########################################################
-- Caricamento e gestione dei dati
-- #########################################################

{- 
   Azione che permette di caricare un file di testo, legge i dati delle canzoni e li
   trasforma in una lista di Canzone. 
-}
caricaCanzoni :: IO [Canzone]
caricaCanzoni = do
    nomeFile <- chiediNomeFile
    contenuto <- readFile nomeFile
    let canzoni = mapMaybe analizzaCanzone (lines contenuto)
    verificaCanzoni canzoni

{- 
   Funzione che verifica il contenuto del file per assicurarsi che sia
   nel formato valido: Titolo,Artista,Genere,Punteggio:

   - L'argomento è la lista di canzoni.
-}
verificaCanzoni :: [Canzone] -> IO [Canzone]
verificaCanzoni canzoni
    | null canzoni = do
        putStrLn "Errore: il file non contiene dati validi! Riprova."
        caricaCanzoni
    | otherwise = do
        putStrLn "File caricato con successo!"
        return canzoni

{- 
   Azione che richiede all'utente di inserire il nome del file con le canzoni
   e ne effettua una validazione dell'input tramite la funzione validaFile.
-}
chiediNomeFile :: IO FilePath
chiediNomeFile = do
    putStrLn "Inserire il nome del file:"
    nomeFile <- getLine
    esito_lettura <- validaFile nomeFile
    verificaEsito esito_lettura nomeFile
   where
        verificaEsito (Right ()) nomeFile = return nomeFile
        verificaEsito (Left err) _ = do
            putStrLn $ "Errore: " ++ err
            chiediNomeFile

{- 
   Funzione che controlla se il nome del file è espresso
   correttamente e se tale file esiste:

   - L'argomento è il nome del file, rappresentato come una stringa.
-}
validaFile :: FilePath -> IO (Either String ())
validaFile nomeFile =
        catch (readFile nomeFile >> return (Right ())) handler
    where
        handler e
            | isDoesNotExistError e = return $ Left "File non trovato!"
            | otherwise = return $ Left "Errore durante l'apertura del file."

{- Azione che effettua la validazione del peso inserito dall'utente. -}
leggiPesoValido :: IO Double
leggiPesoValido = do
    input <- getLine
    let peso = readMaybe input :: Maybe Double
    controllaPeso peso
   where
        controllaPeso (Just p) | p > 0 = return p
        controllaPeso _ = putStrLn "Peso non valido. Riprova." >> leggiPesoValido

{- 
   Funzione che permette all'utente di scegliere
   i generi preferiti e assegnare un peso a ciascuno di essi.
   
   Gli argomenti sono: 
    - Una lista opzionale di canzoni.
    - Una mappa che contiene i pesi attuali per i generi

-}
selezionaGeneriPreferitiEImpostaPesi :: Maybe [Canzone] -> PesiGeneri -> IO PesiGeneri
selezionaGeneriPreferitiEImpostaPesi Nothing pesi = do
    putStrLn "Errore: nessun file caricato. Carica un file prima di continuare."
    return pesi
selezionaGeneriPreferitiEImpostaPesi (Just canzoni) pesi = do
    let generiDisponibili = nub $ map genere canzoni
    putStrLn $ "Generi disponibili: " ++ intercalate ", " generiDisponibili
    generiSelezionati <- raccogliGeneri generiDisponibili
    aggiornaPesi generiSelezionati pesi

{- 
   Funzione che consente all'utente di inserire i generi
   preferiti uno alla volta, terminando con "fine".

   - L'argomento è una lista di generi disponibili, 
     in formato stringa.
-}
raccogliGeneri :: [String] -> IO [String]
raccogliGeneri generiDisponibili = do
    putStrLn "Inserisci i generi preferiti uno alla volta. Scrivi 'fine' per terminare."
    loop []
  where
    loop acc = do
        putStrLn "Inserisci un genere preferito:"
        input <- getLine
        verificaInput input
       where
            verificaInput "fine" = return (nub acc)
            verificaInput i
                | i `elem` generiDisponibili = do
                    putStrLn $ "Genere '" ++ i ++ "' aggiunto ai preferiti."
                    loop (i : acc)
                | otherwise = do
                    putStrLn "Genere non valido. Riprova."
                    loop acc

{- 
   Funzione che consente all'utente di modificare i pesi dei generi preferiti.
   Se il genere ha già un peso, l'utente può scegliere di mantenerlo o aggiornarlo. 
   
   Gli argomenti sono:
    - Una lista di generi da aggiornare, in formato stringa
    - Una mappa che associa ogni genere a un peso.
   
   Caso base: quando la lista dei generi è vuota,
              la funzione restituisce i pesi invariati

   Caso generale: quando la lista dei generi non è vuota,
                  la funzione consente all'utente di scegliere se mantenere o
                  aggiornare il peso di ciascun genere.
-}
aggiornaPesi :: [String] -> PesiGeneri -> IO PesiGeneri
aggiornaPesi [] pesi = return pesi
aggiornaPesi (g:gs) pesi = do
    let pesoCorrente = Map.findWithDefault 1.0 g pesi
    putStrLn $ "Peso corrente per il genere '" ++ g ++ "': " ++ show pesoCorrente
    putStrLn "Vuoi aggiornare il peso? (s/N)"
    risposta <- getLine
    controllaRisposta risposta
   where
        controllaRisposta r
            | r == "s" = do
                putStrLn $ "Inserisci il nuovo peso per il genere '" ++ g ++ "':"
                nuovoPeso <- leggiPesoValido
                aggiornaPesi gs (Map.insert g nuovoPeso pesi)
            | otherwise = do
                putStrLn $ "Peso per il genere '" ++ g ++ "' invariato."
                aggiornaPesi gs pesi

-- #########################################################
-- Raccomandazioni
-- #########################################################

{- 
   Funzione che genera e stampa una lista di canzoni consigliate
   basandosi sui pesi dei generi e sui punteggi delle canzoni.
   
   Gli argomenti sono:
    - Una lista opzionale di canzoni.
    - Una mappa che associa ogni genere a un peso.
   
   Caso base: Se non viene passato nulla, viene visualizzato un messaggio
              di errore che indica che il file non è stato caricato.

    Caso generale: Se viene passata la lista delle canzoni, la funzione ne genera
                   una di canzoni raccomandate in base ai pesi dei generi.
                   Se la lista di canzoni raccomandate è vuota,
                   viene mostrato un messaggio di avviso.
                   Altrimenti, la classifica delle canzoni raccomandate viene stampata.
-}
raccomandaCanzoni :: Maybe [Canzone] -> PesiGeneri -> IO ()
raccomandaCanzoni Nothing _ = putStrLn "Errore: nessun file caricato. Carica un file prima di continuare."
raccomandaCanzoni (Just canzoni) pesi = do
    let raccomandate = raccomanda pesi canzoni
    controllaRaccomandate raccomandate
   where
        controllaRaccomandate r
            | null r = putStrLn "Nessuna canzone trovata con i pesi attuali."
            | otherwise = stampaClassifica r

-- #########################################################
-- Funzioni ausiliarie
-- #########################################################

{- 
   Funzione che converte una riga di testo in un oggetto Canzone.
   restituisce Nothing se la riga non è formattata correttamente. 

   - L'argomento è una stringa che rappresenta la canzone nel formato
     (Titolo,Artista,Genere,Punteggio)
-}
analizzaCanzone :: String -> Maybe Canzone
analizzaCanzone riga = match (separa ',' riga)
   where
    match [titolo, artista, genere, punteggioStr]
        | not (null titolo) && not (null artista) && not (null genere) && not (null punteggioStr)
        , Just punteggio <- readMaybe punteggioStr
        , punteggio >= 1 && punteggio <= 10 = Just (Canzone titolo artista genere punteggio)
    match _ = Nothing

{- 
    Funzione che divide una stringa in una lista di stringhe, usando un delimitatore.
    
    Gli argomenti sono:
     - Il delimitatore che viene utilizzato per separare la stringa.
     - La stringa da dividere in sottostringhe.
    
    Caso base: Se la stringa di input è vuota, la funzione restituisce una lista vuota.

    Caso generale: La funzione divide la stringa nel punto in cui trova il delimitatore.
                   La parte prima del delimitatore viene aggiunta alla lista risultante,
                   e la funzione continua a separare la parte restante della stringa.
                   Se ci sono più delimitatori consecutivi, vengono saltati e la separazione continua.
-}
separa :: Char -> String -> [String]
separa _ "" = []
separa delimiter string =
    let (primo, resto) = break (== delimiter) string
    in primo : separaResto resto
   where
        separaResto [] = []
        separaResto r
            | null r = []
            | otherwise = separa delimiter (dropWhile (== delimiter) (tail r))

{- 
    Funzione che divide una stringa in campi separati, pulendo gli spazi.
    
    Gli argomenti sono:
     - Il delimitatore che viene utilizzato per separare la stringa.
     - La stringa da dividere in sottostringhe.
-}
separaTaglia :: Char -> String -> [String]
separaTaglia delimiter string = map (filter (/= ' ')) (separa delimiter string)

{-
    Funzione che calcola il punteggio ponderato per ogni canzone e le ordina. 
    
    Gli argomenti sono:
     - Una mappa che associa ad ogni genere un peso.
     - Una lista di canzoni da ordinare in base al punteggio ponderato.
-}
raccomanda :: PesiGeneri -> [Canzone] -> [(Double, Canzone)]
raccomanda pesi canzoni =
    let arricchite = arricchisci pesi canzoni
    in sortOn (Down . fst) arricchite

{- 
    Funzione che calcola il punteggio ponderato per ogni canzone.

    Gli argomenti sono:
     - PUna mappa che associa ad ogni genere un peso.
     - Una lista di canzoni per le quali calcolare il punteggio ponderato.
-}
arricchisci :: PesiGeneri -> [Canzone] -> [(Double, Canzone)]
arricchisci pesi canzoni =
    [ (fromIntegral (punteggio c) * Map.findWithDefault 1.0 (genere c) pesi, c) | c <- canzoni ]

-- #########################################################
-- Stampe
-- #########################################################

{- 
    Funzione che stampa le canzoni ordinate con il loro punteggio ponderato.

    - L'argomento è una lista di canzoni con i rispettivi punteggi ponderati.
-}
stampaClassifica :: [(Double, Canzone)] -> IO ()
stampaClassifica raccomandate =
    mapM_ stampaConPosizione (zip [1..] raccomandate)
    where
        stampaConPosizione (pos, (punteggioPonderato, Canzone titolo artista genere _)) = do
            putStrLn $ show pos ++ "# " ++ titolo ++ " (Artista: " ++ artista ++ ", Genere: " ++ genere ++ ", Punteggio Ponderato: " ++ show punteggioPonderato

{- 
    Funzione che permette di visualizzare i generi preferiti e i pesi associati.

    - L'argomento è una mappa che associa ad ogni genere un peso. 
-}
visualizzaGeneriPreferiti :: PesiGeneri -> IO ()
visualizzaGeneriPreferiti pesi
    | Map.null pesi = putStrLn "Nessun genere ancora definito."
    | otherwise = do
        putStrLn "I tuoi generi preferiti e pesi associati sono:"
        mapM_ stampaGenere (Map.toList pesi)

{- 
    Funzione che stampa il genere, concatenato al peso suo relativo
    
    - L'argomento è una coppia di valori (String, Double),
    dove il primo elemento è il nome del genere e il secondo è il peso associato.
-}
stampaGenere :: (String, Double) -> IO ()
stampaGenere (genere, peso) = putStrLn $ genere ++ ": " ++ show peso
