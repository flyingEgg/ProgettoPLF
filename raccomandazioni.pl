/* ######################################################### */
/* # Corso di Programmazione Logica e Funzionale           # */
/* # Progetto di raccomandazione di canzoni                # */
/* # Studente: Giaconi Christian, Giacomo Rossi            # */
/* # Matricola: 314045, 314671                             # */
/* ######################################################### */

/*
    Specifica: scrivere un programma Prolog per implementare un sistema di raccoandazione di canzoni.
    Il programma legge le canzoni espresse in quattro attributi: titolo, artista,
    genere musicale e punteggio; da un file con valori separati da virgole, il cui nome viene acquisito
    da tastiera e le suggerisce all’utente basandosi sulle sue preferenze musicali, utilizzando un
    punteggio di gradimento acquisito da tastiera e configurato su uno o più generi, per creare una
    classifica con le canzoni ordinate secondo il punteggio ponderato da quello di gradimento del
    o dei generi musicali.
*/

/* ================================================
   Predicati dinamici
   ================================================ */

/* Predicato che 'canzone/4' memorizza informazioni relative
   alle canzoni caricate dal file. Ogni canzone è rappresentata 
   dai seguenti argomenti: Titolo, Artista, Genere e Punteggio. */
:- dynamic(canzone/4).

/* Predicato che 'genere_preferito/2' associa un peso preferito
   a ciascun genere musicale. Il primo argomento è il Genere,
   il secondo è il Peso associato a quel genere. */
:- dynamic(genere_preferito/2).

/* Predicato che 'stringa/1' memorizzi una riga del file 
   con le canzoni per poi operarvici di conseguenza con i
   predicati di memorizzazione delle canzoni. */
:- dynamic(stringa/1).

/* ================================================
   Predicati principali
   ================================================ */

/* Predicato che funge da punto di ingresso principale.
   Inizializza il programma e avvia il menu interattivo
   per l'utente. */
main :-
    loop_menu.

/* Predicato che gestisce la selezione delle azioni da parte 
   dell'utente nel menu principale. Ogni opzione del menu chiama
   un predicato specifico per eseguire l'azione corrispondente. */
loop_menu :-
    write('\n--- Sistema di Raccomandazione Musicale ---\n'),
    write('1. Carica un file con le canzoni\n'),
    write('2. Gestisci i generi preferiti (aggiungi o modifica)\n'),
    write('3. Stampa la classifica delle canzoni\n'),
    write('4. Stampa i generi preferiti con il relativo punteggio\n'),
    write('5. Esci\n'),
    write('Seleziona un\'opzione:\n'),
    read(Scelta),
    (   Scelta = 1 -> carica_canzoni_interattivo
    ;   Scelta = 2 -> gestisci_generi_preferiti
    ;   Scelta = 3 -> stampa_classifica
    ;   Scelta = 4 -> mostra_generi_preferiti
    ;   Scelta = 5 -> write('Arrivederci!\n'), halt
    ;   write('Scelta non valida. Riprova.\n')
    ),
    loop_menu.

/* ================================================
   Predicati per il caricamento delle canzoni
   ================================================ */

/* Predicato che permette all'utente di inserire il nome del
   file che contiene le canzoni.
   Se il caricamento ha successo, stampa un messaggio di conferma. */
carica_canzoni_interattivo :-
    chiedi_nome_file(File),
    carica_file(File).

chiedi_nome_file(File) :-
    write('Inserire tra apici il nome del file contenente le canzoni: '), nl,
    read(File).

carica_file(File) :-
    catch(carica_canzoni(File), _, fallimento_caricamento),
    successo_caricamento.

successo_caricamento :-
    write('\nFile caricato con successo!\n').

fallimento_caricamento :-
    write('\nErrore nel caricamento del file. Riprova.\n'),
    carica_canzoni_interattivo.



/* Predicato che analizza il contenuto del file e ne
   "processa" il contenuto. 

   Gli argomenti sono:
     - Il percorso del file che contiene i dati delle canzoni.   
*/
carica_canzoni(File) :-
    open(File, read, Stream),
    leggi_righe(Stream, Lines),
    close(Stream),
    elabora_righe(Lines).

/* 
    Predicato per leggere le righe del file

    Gli argomenti sono:
     - Lo stream di lettura del file.
     - La lista che conterrà le righe lette.

    Caso base: Quando si raggiunge la fine del file,
               il predicato termina e la lista risultante è vuota.

    Caso generale: Se non si è ancora alla fine del file, 
                   si legge un carattere dallo stream e lo si aggiunge alla lista, 
                   continuando a leggere il resto del file ricorsivamente.
*/
leggi_righe(Stream,[]):-
    at_end_of_stream(Stream).

leggi_righe(Stream,[X|L]):-
    \+ at_end_of_stream(Stream),
    get_char(Stream,X),
    leggi_righe(Stream,L).

/* 
    Predicato che processa e avvia il parsing delle righe.

    Gli argomenti sono:
     - La lista delle righe (una lista di caratteri).
     
    Caso base: Quando la lista è vuota, il predicato termina senza fare nulla.

    Caso generale: Quando ci sono ancora righe da processare,
                   il predicato estrae la prima riga dalla lista,
                   la analizza, esegue il parsing della stringa risultante, 
                   e poi ricorsivamente processa le righe rimanenti.    
*/
elabora_righe([]) :- !.

elabora_righe([Char | Rest]) :-
    elabora_riga_singola(Rest, [Char], Stringa, RestDopoLinea),
    parsing_righe(Stringa),
    elabora_righe(RestDopoLinea).

/* Predicato che processa una singola riga.

   Gli argomenti sono:
    - La lista di caratteri che rappresenta la riga da processare.
    - Un accumulatore che tiene traccia dei caratteri letti finora.
    - Una variabile `Stringa` che conterrà la riga completata una volta processata.
    - Una lista di caratteri rimanenti dopo il trattamento della riga (questo viene usato per la ricorsione).

    Caso base 1: Quando la lista di caratteri è vuota, significa che la riga è stata interamente letta.
                 Viene quindi creato un atomo con l'accumulatore.

    Caso base 2: Quando viene trovato un carattere di nuova linea, viene completata la riga
                 e viene creato un atomo con l'accumulatore,
                 mentre la lista di caratteri rimanenti è restituita come `Rest`.

    Caso generale: Quando un carattere non è una nuova linea, il carattere viene
                   aggiunto all'accumulatore, e il predicato ricorsivamente processa
                   i caratteri rimanenti fino al termine della riga.
*/
elabora_riga_singola([], Accumulatore, Stringa, []) :-
    atom_chars(Stringa, Accumulatore).

elabora_riga_singola(['\n' | Rest], Accumulatore, Stringa, Rest) :-
    atom_chars(Stringa, Accumulatore).

elabora_riga_singola([Char | Rest], Accumulatore, Stringa, RestDopoLinea) :-
    Char \= '\n',
    append(Accumulatore, [Char], NuovoAccumulatore),
    elabora_riga_singola(Rest, NuovoAccumulatore, Stringa, RestDopoLinea).

/* 
    Predicato che effettua il parsing delle righe.

    Gli argomenti sono:
     - La riga contenente i dati della canzone da analizzare.
*/
parsing_righe(Riga) :-
    separa_stringa(Riga, ',', '', [Titolo, Artista, Genere, PunteggioStr]),
    string_trim(PunteggioStr, TrimmedPunteggioStr),
    valida_e_inserisci(Titolo, Artista, Genere, TrimmedPunteggioStr).

/*
    Predicato che valida il punteggio della canzone e la salva
    in un nuovo record se la validazine ha esito positivo o
    lancia un errore con relativo messaggio altrimenti.

    Gli argomenti sono:
    - Titolo, artista e genere musicale della canzone
    - ???
*/
valida_e_inserisci(Titolo, Artista, Genere, TrimmedPunteggioStr) :-
    number_string(Punteggio, TrimmedPunteggioStr),
    assertz(canzone(Titolo, Artista, Genere, Punteggio)).
valida_e_inserisci(_, _, _, PunteggioStr) :-
    format('Errore nella conversione del punteggio: ~w\n', [PunteggioStr]),
    fail.

    
/* ================================================
   Predicati ausiliari per la gestione delle stringhe
   ================================================ */

/* 
    Predicato che permette di creare un numero a partire da una stringa.
    Viene utilizzato quando il primo argomento (Number) è una variabile
    e il predicato tenta di inferire il valore della variabile a partire dalla stringa.

    Gli argomenti sono:
     - Il numero da ottenere o assegnare alla variabile.
     - La stringa contenente la rappresentazione del numero.

    Caso 1: Quando `Number` è una variabile, il predicato tenta di inferire il valore della variabile
                 a partire dalla stringa `String` utilizzando il predicato `atom_codes` per convertire la stringa
                 in una lista di codici e poi `number_codes` per ottenere il numero corrispondente.

    Caso 2: Quando `Number` è già un numero, il predicato converte il numero in una lista di codici
                 e poi utilizza `atom_codes` per ottenere la stringa corrispondente al numero.    
*/
number_string(Number, String) :-
    var(Number), !,
    atom_codes(String, Codes),
    catch(number_codes(Number, Codes), _, fail).

number_string(Number, String) :-
    number(Number), !,
    number_codes(Number, Codes),
    atom_codes(String, Codes).

/* 
    Predicato rimuove gli spazi da una stringa.

    Gli argomenti sono:
     - La stringa di input dalla quale si vogliono rimuovere gli spazi.
     - La stringa risultante, senza spazi bianchi, tabulazioni, nuove linee e ritorni a capo.
*/
string_trim(String, Trimmed) :-
    separa_stringa(String, '', ' \t\n\r', [Trimmed|_]).

/* 
    Predicato che divide una stringa in sottostringhe in base a un separatore e a un padding.
    String: stringa di input da suddividere.
    Separator: caratteri che fungono da separatori tra le sottostringhe.
    Padding: caratteri da ignorare all'inizio e alla fine delle sottostringhe.
    Substrings: lista delle sottostringhe risultanti dalla suddivisione.

   Gli argomenti sono:
     - La stringa di input da suddividere in sottostringhe.
     - I caratteri che fungono da separatori tra le sottostringhe.
     - I caratteri da ignorare all'inizio e alla fine delle sottostringhe.
     - La lista delle sottostringhe risultanti dalla suddivisione.   
*/
separa_stringa(String, Separator, Padding, Substrings) :-
    atom_codes(String, StringCodes),
    atom_codes(Separator, SeparatorCodes),
    atom_codes(Padding, PaddingCodes),
    separa_codici_stringa(StringCodes, SeparatorCodes, PaddingCodes, Substrings).

/* 
    Predicato che suddivide una stringa in sottostringhe, eliminando il padding
    e utilizzando il separatore specificato.
    Restituisce la lista di sottostringhe una alla volta.

    Gli argomenti sono:
     - La lista dei codici di caratteri della stringa di input.
     - La lista dei codici di caratteri che fungono da separatori.
     - La lista dei codici di caratteri che devono essere ignorati (padding).
     - La lista risultante di sottostringhe (ogni sottostringa è una lista di codici di caratteri).

    Caso base: Se la stringa di input è vuota, la lista di sottostringhe risultante è vuota.
    
    Caso generale: Il predicato chiama il predicato ausiliario `separa_codici_stringa_aux`
                   per separare la stringa in sottostringhe, 
                   rimuovendo il padding e utilizzando il separatore specificato. 
                   Ogni sottostringa viene poi convertita da una lista di codici di caratteri 
                   in un atomo e aggiunta alla lista risultante.
                   La funzione continua a chiamarsi ricorsivamente per il resto della stringa.
*/
separa_codici_stringa([], _, _, []) :- !.

separa_codici_stringa(StringCodes, SeparatorCodes, PaddingCodes, [Substring|Substrings]) :-
    separa_codici_stringa_aux(StringCodes, SeparatorCodes, PaddingCodes, SubstringCodes, RestCodes),
    atom_codes(Substring, SubstringCodes),
    separa_codici_stringa(RestCodes, SeparatorCodes, PaddingCodes, Substrings).

/* 
    Predicato ausiliario che rileva un separatore all'inizio della stringa.
    Quando viene trovato un separatore, termina l'estrazione della sottostringa corrente
    e restituisce i caratteri rimanenti per l'elaborazione successiva.

    Gli argomenti sono:
     - La lista dei codici di caratteri della stringa da elaborare.
     - La lista dei codici di caratteri che fungono da separatori.
     - La lista dei codici di caratteri da ignorare (padding).
     - La lista di codici di caratteri che costituiscono la sottostringa estratta.
     - La lista di codici di caratteri rimanenti da elaborare.

    Caso base 1: Se la lista di codici di caratteri è vuota,
                 restituisce una lista vuota per la sottostringa
                 e una lista vuota per i caratteri rimanenti.
    
    Caso base 2: Se viene trovato un separatore all'inizio della stringa,
                 la sottostringa viene completata (lista vuota) e il
                 resto della stringa (senza il separatore) viene restituito.

    Caso generale 1: Se il carattere non è né un separatore né un padding,
                     viene aggiunto alla sottostringa e il predicato
                     continua a processare i caratteri successivi.

    Caso generale 2: Se il carattere è un padding, viene ignorato e
                     il predicato continua a processare i caratteri successivi.
*/
separa_codici_stringa_aux([], _, _, [], []) :- !.

separa_codici_stringa_aux([C|Cs], SeparatorCodes, _, [], Cs) :-
    member(C, SeparatorCodes), !.

separa_codici_stringa_aux([C|Cs], SeparatorCodes, PaddingCodes, [C|SubstringCodes], RestCodes) :-
    \+ member(C, SeparatorCodes),
    \+ member(C, PaddingCodes), !,
    separa_codici_stringa_aux(Cs, SeparatorCodes, PaddingCodes, SubstringCodes, RestCodes).

separa_codici_stringa_aux([C|Cs], SeparatorCodes, PaddingCodes, SubstringCodes, RestCodes) :-
    member(C, PaddingCodes), !,
    separa_codici_stringa_aux(Cs, SeparatorCodes, PaddingCodes, SubstringCodes, RestCodes).

/* ================================================
   Predicati per la gestione dei generi preferiti
   ================================================ */

/* 
    Predicato che permette all'utente di selezionare e gestire i generi musicali preferiti.    
*/
gestisci_generi_preferiti :- 
    ottieni_generi_disponibili(GeneriDisponibili),
    controllo_generi_disponibili(GeneriDisponibili).

controllo_generi_disponibili([]) :-
    write('Non ci sono generi disponibili. Impossibile inserire preferenze.\n').
controllo_generi_disponibili(GeneriDisponibili) :-
    mostra_generi_disponibili(GeneriDisponibili),
    write('Inserisci i tuoi generi preferiti, uno per volta. Scrivi "fine" per terminare.\n'),
    chiedi_generi_preferiti([]).

/* 
    Predicato che raccoglie i generi preferiti inseriti dall'utente e li aggiunge alla lista di preferiti.

    Gli argomenti sono:
     - GeneriPreferiti: una lista che contiene i generi musicali preferiti dall'utente.
*/
chiedi_generi_preferiti(GeneriPreferiti) :- 
    ottieni_generi_disponibili(GeneriDisponibili),
    write('Inserisci un genere preferito: '),
    read(Genere),
    (   Genere == fine
    ->  write('\nVuoi aggiornare i pesi? (\'s\'/\'n\')\n'),
        read(Risposta),
        (   Risposta == 's'
        ->  chiedi_peso_generi(GeneriPreferiti)
        ;   write('\nPeso invariato\n')
        )
    ;   normalizza_genere(Genere, GenereNormalizzato),
        maplist(normalizza_genere, GeneriDisponibili, GeneriDisponibiliNormalizzati),
        (   member(GenereNormalizzato, GeneriDisponibiliNormalizzati)
        ->  (   member(GenereNormalizzato, GeneriPreferiti)
            ->  write('Questo genere è già presente tra i tuoi preferiti.\n'),
                chiedi_generi_preferiti(GeneriPreferiti)
            ;   append(GeneriPreferiti, [GenereNormalizzato], NuoviGeneri),
                write('Genere aggiunto con successo.\n'),
                chiedi_generi_preferiti(NuoviGeneri)
            )
        ;   write('Genere non valido. Riprova.\n'),
            chiedi_generi_preferiti(GeneriPreferiti)
        )
    ).

/* 
    Predicato che chiede all'utente di inserire
    un peso per ciascun genere musicale preferito.

    li argomenti sono:
     - La lista dei generi preferiti.

    Caso base: Quando la lista dei generi è vuota, il predicato termina senza fare nulla.

    Caso generale: Quando ci sono ancora generi da processare,
                   il predicato chiede il peso per ogni genere, lo valida,
                   aggiorna la conoscenza e poi ricorsivamente gestisce il prossimo genere.
*/
chiedi_peso_generi([]).

chiedi_peso_generi([Genere | Altri]) :- 
    format('Inserisci il peso per il genere ~w: ', [Genere]),
    read(Peso),
    (   number(Peso), Peso > 0
    ->  (    retract(genere_preferito(Genere, _))
        -> true ; true
        ),
        assertz(genere_preferito(Genere, Peso)), chiedi_peso_generi(Altri)
        ;   write('Peso non valido. Riprova.\n'), chiedi_peso_generi([Genere | Altri])
    ).
    
/* ================================================
   Predicati per la raccomandazione e la classifica
   ================================================ */

/* 
    Predicato che stampa le canzoni ordinate in base al punteggio
    ponderato, elencandole con la posizione, il titolo,
    l'artista e il punteggio ponderato.

    Gli argomenti sono:
     - La lista delle canzoni ordinate (una lista di coppie formato
       (PunteggioPonderato, Titolo)).
     - La posizione corrente nella lista da stampare.

    Caso base: Quando la lista è vuota, il predicato termina senza fare nulla.

    Caso generale: Quando ci sono ancora canzoni da stampare,
                   il predicato estrae la prima canzone dalla lista,
                   recupera i suoi dettagli (titolo, artista, genere),
                   e poi ricorsivamente stampa le canzoni rimanenti.
*/
stampa_canzoni_ordinate([], _).

stampa_canzoni_ordinate([PunteggioPonderato-Titolo | Rest], Posizione) :- 
    canzone(Titolo, Artista, Genere, _),
    format('~d# ~w (Artista: ~w, Genere: ~w, Punteggio ponderato: ~2f)\n', 
            [Posizione, Titolo, Artista, Genere, PunteggioPonderato]),
    stampa_canzoni_ordinate(Rest, Posizione + 1).

/* 
    Predicato che calcola il punteggio ponderato per ogni canzone 
    in base al suo genere e al suo punteggio originale.
    Poi stampa la classifica ordinata delle canzoni.
*/
stampa_classifica :-
    findall(PunteggioPonderato-Titolo, calcola_punteggio_ponderato(Titolo, PunteggioPonderato), PunteggiModificati),
    findall(Punteggio-Titolo, (canzone(Titolo, _, _, Punteggio), \+ member(_-Titolo, PunteggiModificati)), PunteggiInvariati),
    append(PunteggiModificati, PunteggiInvariati, PunteggiTotali),
    sort(PunteggiTotali, PunteggiOrdinatiAsc),
    reverse(PunteggiOrdinatiAsc, PunteggiOrdinati),
    controllo_punteggi_totali(PunteggiTotali, PunteggiOrdinati).

controllo_punteggi_totali([], _) :-
    write('Nessuna canzone trovata con punteggio ponderato.\n').
controllo_punteggi_totali(PunteggiOrdinati, _) :-
    write('\nClassifica delle canzoni ordinate per punteggio:\n'),
    stampa_canzoni_ordinate(PunteggiOrdinati, 1).
/* 
    Predicato che calcola il punteggio ponderato
    di una canzone in base al suo genere (e al peso preferito associato)
    moltiplicato per il punteggio originale della canzone.

    Gli argomenti sono:
     - Il titolo della canzone.
     - Il punteggio della canzone dopo l'applicazione del peso.
*/
calcola_punteggio_ponderato(Titolo, PunteggioPonderato) :- 
    canzone(Titolo, _, Genere, Punteggio),
    normalizza_genere(Genere, GenereNormalizzato),
    peso_genere(GenereNormalizzato, Peso),
    Peso \= 1,
    PunteggioPonderato is Punteggio * Peso.
    
/* ================================================
   Predicati ausiliari
   ================================================ */

/* 
    Predicato che mostra i generi preferiti associati
    con il rispettivo peso.
*/
mostra_generi_preferiti :- 
    findall(Genere-Peso, genere_preferito(Genere, Peso), Generi),
    controllo_generi(Generi).

    controllo_generi([]) :-
        write('Non è stato definito alcun genere preferito.\n').
    controllo_generi(Generi) :-
        write('I tuoi generi preferiti e i loro pesi:\n'),
        stampa_generi(Generi).

/* 
    Predicato che restituisce i generi disponibili

    Gli argomenti sono:
     - GeneriDisponibili: lista dei generi musicali disponibili, ordinata senza duplicati.
*/
ottieni_generi_disponibili(GeneriDisponibili):-
    findall(Genere, canzone(_, _, Genere, _), Generi),
    sort(Generi, GeneriOrdinati),
    GeneriDisponibili = GeneriOrdinati.

/* 
    Predicato che stampa a video una lista dei generi musicali 
    univoci presenti nel database delle canzoni.
*/
mostra_generi_disponibili(GeneriOrdinati):-
    format('Generi disponibili: ~w\n', [GeneriOrdinati]).
    
/* 
    Predicato che stampa la lista dei generi preferiti.

    Gli argomenti sono:
     - La lista di generi musicali con il peso associato (Lista dei preferiti).
    
    Caso base: Quando la lista è vuota, il predicato termina senza fare nulla.
    Caso generale: Quando ci sono ancora generi nella lista,
                   il predicato stampa il genere e il suo peso,
                   quindi chiama ricorsivamente se stesso per stampare i restanti.
*/
stampa_generi([]).

stampa_generi([Genere-Peso | Rest]) :- 
    format('~w: ~w\n', [Genere, Peso]),
    stampa_generi(Rest).
    
/* 
    Predicato che normalizza un genere, trasformandolo in minuscolo
    e rimuovendo gli spazi. Questo permette di confrontare i generi
    in modo case-insensitive e indipendentemente dalla formattazione.

    Gli argomenti sono:
     - Il genere musicale in input.
     - Il genere musicale normalizzato in output.
*/
normalizza_genere(Genere, GenereNormalizzato) :-
    (atom(Genere) -> true ; atom_codes(Genere, Genere)),
    carattere_atomo_minuscolo(Genere, GenereLower),
    atom_codes(GenereLower, Codici),
    rimuovi_spazi(Codici, CodiciNormalizzati),
    atom_codes(GenereNormalizzato, CodiciNormalizzati).

/* 
    Predicato che converte tutti i caratteri di un atomo in minuscolo.

    Gli argomenti sono:
     - L'atomo in ingresso.
     - L'atomo in uscita, trasformato in minuscolo.
*/
carattere_atomo_minuscolo(Atom, LowercaseAtom) :-
    atom_codes(Atom, Codes),
    maplist(to_lower, Codes, LowercaseCodes),
    atom_codes(LowercaseAtom, LowercaseCodes).

/* 
    Predicato che converte un singolo carattere in minuscolo.

    Gli argomenti sono:
     - Il codice del carattere in ingresso.
     - Il codice del carattere in uscita, trasformato in minuscolo.
    
    Caso base: Quando il codice è già in minuscolo, viene restituito senza modifiche.
    
    Caso generale: Quando il codice corrisponde a una lettera maiuscola (tra 65 e 90 nel codice ASCII),
                   viene convertito nel corrispondente codice di una lettera minuscola (aggiungendo 32).
*/
to_lower(Code, LowerCode) :-
    Code >= 65, Code =< 90,
    LowerCode is Code + 32.

to_lower(Code, Code).

/* 
    Predicato che restituisce il peso associato ad un genere. Se non è
    stato definito un peso specifico, viene utilizzato un peso predefinito (1).

    Gli argomenti sono:
     - Il genere musicale.
     - Il peso associato al genere.
*/
peso_genere(Genere, Peso) :- 
    (   genere_preferito(Genere, Peso) -> true ; Peso = 1 ).

/* 
    Predicato che rimuove tutti gli spazi da una stringa, sia che sia
    rappresentata come atomo o come lista di codici ASCII.

    Gli argomenti sono:
     - La stringa di input, che può essere un atomo o una lista di codici ASCII.
     - La stringa risultante, senza spazi.
*/
rimuovi_spazi(Stringa, StringaRimossa) :-
    (    atom(Stringa) -> atom_codes(Stringa, Codici) ; Codici = Stringa ),
    rimuovi_spazi_codici(Codici, CodiciRimossi),
    (    atom(Stringa) -> atom_codes(StringaRimossa, CodiciRimossi) ; StringaRimossa = CodiciRimossi ).

/* 
    Predicato ausiliario che rimuove gli spazi (codice ASCII 32)
    da una lista di codici ASCII.

    Gli argomenti sono:
     - La lista di codici ASCII da elaborare.
     - La lista risultante, che sarà la stessa lista senza gli spazi.

    Caso base: Quando la lista di input è vuota, la lista risultante è anch'essa vuota.
    
    Caso ricorsivo: Quando il primo elemento della lista è uno spazio (ASCII 32),
                    viene ignorato e il predicato continua a processare il resto della lista.
                    Se il primo elemento non è uno spazio, viene mantenuto nella lista risultante.
*/
rimuovi_spazi_codici([], []).
rimuovi_spazi_codici([32|T], R) :- rimuovi_spazi_codici(T, R).
rimuovi_spazi_codici([H|T], [H|R]) :- H \= 32, rimuovi_spazi_codici(T, R).
