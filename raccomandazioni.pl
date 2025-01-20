/* ######################################################### */
/* # Corso di Programmazione Logica e Funzionale           # */
/* # Progetto di raccomandazione di canzoni                # */
/* # Studente: Giaconi Christian, Giacomo Rossi            # */
/* # Matricola: 314045, 314671                             # */
/* ######################################################### */

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
    write('\n--- Sistema di Raccomandazione Musicale ---\n'),
    loop_menu.

/* Predicato che gestisce la selezione delle azioni da parte 
   dell'utente nel menu principale. Ogni opzione del menu chiama
   un predicato specifico per eseguire l'azione corrispondente. */
loop_menu :-
    write('1. Carica un file con le canzoni\n'),
    write('2. Gestisci i generi preferiti (aggiungi o modifica)\n'),
    write('3. Stampa la classifica delle canzoni\n'),
    write('4. Stampa la lista dei generi preferiti\n'),
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
    write('Inserire tra apici il nome del file contenente le canzoni: '), nl,
    read(File),
    (   catch(carica_canzoni(File), _, fail)
    ->  write('\n\nCanzoni caricate con successo!\n')
    ;   write('\nErrore nel caricamento del file. Riprova.\n')
    ).

/* Predicato che analizza il contenuto del file e ne
   "processa" il contenuto. */
carica_canzoni(File) :-
    open(File, read, Stream),
    leggi_righe(Stream, Lines),
    close(Stream),
    processa_righe(Lines).

/* Predicato per leggere le  */
leggi_righe(Stream,[]):-
    at_end_of_stream(Stream).

leggi_righe(Stream,[X|L]):-
    \+ at_end_of_stream(Stream),
    get_char(Stream,X),
    leggi_righe(Stream,L).

/* Predicato che processa e avvia il parsing delle righe. */
processa_righe([]) :- !.

processa_righe([Char | Rest]) :-
    processa_riga_singola(Rest, [Char], Stringa, RestDopoLinea),
    parsing_righe(Stringa),
    processa_righe(RestDopoLinea).

/* Predicato che provcessa una singola riga. */
processa_riga_singola([], Accumulatore, Stringa, []) :-
    atom_chars(Stringa, Accumulatore).

processa_riga_singola(['\n' | Rest], Accumulatore, Stringa, Rest) :-
    atom_chars(Stringa, Accumulatore).

processa_riga_singola([Char | Rest], Accumulatore, Stringa, RestDopoLinea) :-
    Char \= '\n',
    append(Accumulatore, [Char], NuovoAccumulatore),
    processa_riga_singola(Rest, NuovoAccumulatore, Stringa, RestDopoLinea).

/* Predicato che effettua il parsing delle righe. */
parsing_righe(Riga) :-
    split_string(Riga, ',', '', [Titolo, Artista, Genere, PunteggioStr]),
        string_trim(PunteggioStr, TrimmedPunteggioStr),
        (   number_string(Punteggio, TrimmedPunteggioStr)
        ->  assertz(canzone(Titolo, Artista, Genere, Punteggio))
        ;   format('Errore nella conversione del punteggio: ~w\n', [PunteggioStr])
        ).
    
/* ================================================
   Predicati ausiliari per la gestione delle stringhe
   ================================================ */

/* Predicato che permette di creare un numero a partire da una stringa.
   Viene utilizzato quando il primo argomento (Number) è una variabile
   e il predicato tenta di inferire il valore della variabile a partire dalla stringa. */
number_string(Number, String) :-
    var(Number), !,
    atom_codes(String, Codes),
    catch(number_codes(Number, Codes), _, fail).

/* Predicato che consente di creare una stringa a partire da un numero.
   Viene utilizzato quando il primo argomento (Number) è già un numero
   e il predicato converte questo numero in una stringa. */
number_string(Number, String) :-
    number(Number), !,
    number_codes(Number, Codes),
    atom_codes(String, Codes).

/* Predicato rimuove gli spazi da una stringa.*/
string_trim(String, Trimmed) :-
    split_string(String, '', ' \t\n\r', [Trimmed|_]).

/* Predicato che divide una stringa in sottostringhe in base a un separatore e a un padding.
   String: stringa di input da suddividere.
   Separator: caratteri che fungono da separatori tra le sottostringhe.
   Padding: caratteri da ignorare all'inizio e alla fine delle sottostringhe.
   Substrings: lista delle sottostringhe risultanti dalla suddivisione. */
split_string(String, Separator, Padding, Substrings) :-
    atom_codes(String, StringCodes),
    atom_codes(Separator, SeparatorCodes),
    atom_codes(Padding, PaddingCodes),
    split_string_codes(StringCodes, SeparatorCodes, PaddingCodes, Substrings).

/* Predicato che suddivide una stringa in sottostringhe, eliminando il padding
   e utilizzando il separatore specificato.
   Restituisce la lista di sottostringhe una alla volta. */
split_string_codes([], _, _, []) :- !.

split_string_codes(StringCodes, SeparatorCodes, PaddingCodes, [Substring|Substrings]) :-
    split_string_codes_aux(StringCodes, SeparatorCodes, PaddingCodes, SubstringCodes, RestCodes),
    atom_codes(Substring, SubstringCodes),
    split_string_codes(RestCodes, SeparatorCodes, PaddingCodes, Substrings).

/* Predicato ausiliario che rileva un separatore all'inizio della stringa.
   Quando viene trovato un separatore, termina l'estrazione della sottostringa corrente
   e restituisce i caratteri rimanenti per l'elaborazione successiva. */
split_string_codes_aux([], _, _, [], []) :- !.

split_string_codes_aux([C|Cs], SeparatorCodes, _, [], Cs) :-
    member(C, SeparatorCodes), !.

split_string_codes_aux([C|Cs], SeparatorCodes, PaddingCodes, [C|SubstringCodes], RestCodes) :-
    \+ member(C, SeparatorCodes),
    \+ member(C, PaddingCodes), !,
    split_string_codes_aux(Cs, SeparatorCodes, PaddingCodes, SubstringCodes, RestCodes).

split_string_codes_aux([C|Cs], SeparatorCodes, PaddingCodes, SubstringCodes, RestCodes) :-
    member(C, PaddingCodes), !,
    split_string_codes_aux(Cs, SeparatorCodes, PaddingCodes, SubstringCodes, RestCodes).

/* ================================================
   Predicati per la gestione dei generi preferiti
   ================================================ */

/* Predicato che permette all'utente di selezionare e gestire i generi musicali preferiti. */
gestisci_generi_preferiti :- 
    ottieni_generi_disponibili(GeneriDisponibili),
    (   GeneriDisponibili == []
    ->  write('Non ci sono generi disponibili. Impossibile inserire preferenze.\n')
    ;   mostra_generi_disponibili,
        write('Inserisci i tuoi generi preferiti, uno per volta. Scrivi "fine" per terminare.\n'),
        chiedi_generi_preferiti([])
    ).

/* Predicato che raccoglie i generi preferiti inseriti dall'utente e li aggiunge alla lista di preferiti. */
chiedi_generi_preferiti(GeneriPreferiti) :- 
    ottieni_generi_disponibili(GeneriDisponibili),
    write('Inserisci un genere preferito: '),
    read(Genere),
    (   Genere == fine
    ->  chiedi_peso_generi(GeneriPreferiti)
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

/* Predicato che chiede all'utente di inserire
   un peso per ciascun genere musicale preferito. */
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

/* Predicato che stampa le canzoni ordinate in base al punteggio
   ponderato, elencandole con la posizione, il titolo,
   l'artista e il punteggio ponderato. */
stampa_canzoni_ordinate([], _).

stampa_canzoni_ordinate([PunteggioPonderato-Titolo | Rest], Posizione) :- 
    canzone(Titolo, Artista, Genere, _),
    format('~d# ~w (Artista: ~w, Genere: ~w, Punteggio ponderato: ~2f)\n', 
            [Posizione, Titolo, Artista, Genere, PunteggioPonderato]),
    stampa_canzoni_ordinate(Rest, Posizione + 1).

/* Predicato che calcola il punteggio ponderato per ogni canzone 
   in base al suo genere e al suo punteggio originale.
   Poi stampa la classifica ordinata delle canzoni. */
stampa_classifica :-
    findall(PunteggioPonderato-Titolo, calcola_punteggio_ponderato(Titolo, PunteggioPonderato), PunteggiModificati),
    findall(Punteggio-Titolo, (canzone(Titolo, _, _, Punteggio), \+ member(_-Titolo, PunteggiModificati)), PunteggiInvariati),
    append(PunteggiModificati, PunteggiInvariati, PunteggiTotali),
    sort(PunteggiTotali, PunteggiOrdinatiAsc),
    reverse(PunteggiOrdinatiAsc, PunteggiOrdinati),
    (   PunteggiTotali == []
    ->  write('Nessuna canzone trovata con punteggio ponderato.\n')
    ;   write('\nClassifica delle canzoni ordinate per punteggio:\n'),
        stampa_canzoni_ordinate(PunteggiOrdinati, 1)
    ).

/* Predicato che calcola il punteggio ponderato
    di una canzone in base al suo genere (e al peso preferito associato)
    moltiplicato per il punteggio originale della canzone. */
calcola_punteggio_ponderato(Titolo, PunteggioPonderato) :- 
    canzone(Titolo, _, Genere, Punteggio),
    normalizza_genere(Genere, GenereNormalizzato),
    peso_genere(GenereNormalizzato, Peso),
    Peso \= 1,
    PunteggioPonderato is Punteggio * Peso.
    
/* ================================================
   Predicati ausiliari
   ================================================ */

/* Predicato che mostra i generi preferiti associati
   con il rispettivo peso. */
mostra_generi_preferiti :- 
    findall(Genere-Peso, genere_preferito(Genere, Peso), Generi),
    (   Generi == []
    ->  write('Non è stato definito alcun genere preferito.\n')
    ;   write('I tuoi generi preferiti e i loro pesi:\n'),
        stampa_generi(Generi)
    ).

/* Predicato che restituisce i generi disponibili */
ottieni_generi_disponibili(GeneriDisponibili):-
    findall(Genere, canzone(_, _, Genere, _), Generi),
    sort(Generi, GeneriOrdinati),
    GeneriDisponibili = GeneriOrdinati.

/* Predicato che stampa a video una lista dei generi musicali 
   univoci presenti nel database delle canzoni. */
mostra_generi_disponibili :-
    ottieni_generi_disponibili(GeneriOrdinati),
    format('Generi disponibili: ~w\n', [GeneriOrdinati]).
    
/* Predicato che stampa la lista dei generi preferiti. */
stampa_generi([]).

stampa_generi([Genere-Peso | Rest]) :- 
    format('~w: ~w\n', [Genere, Peso]),
    stampa_generi(Rest).
    
/* Predicato che normalizza un genere, trasformandolo in minuscolo
   e rimuovendo gli spazi. Questo permette di confrontare i generi
   in modo case-insensitive e indipendentemente dalla formattazione. */
normalizza_genere(Genere, GenereNormalizzato) :-
    (atom(Genere) -> true ; atom_codes(Genere, Genere)),
    downcase_atom(Genere, GenereLower),
    atom_codes(GenereLower, Codici),
    rimuovi_spazi(Codici, CodiciNormalizzati),
    atom_codes(GenereNormalizzato, CodiciNormalizzati).

/* Predicato che converte tutti i caratteri di un atomo in minuscolo. */
downcase_atom(Atom, LowercaseAtom) :-
    atom_codes(Atom, Codes),
    maplist(to_lower, Codes, LowercaseCodes),
    atom_codes(LowercaseAtom, LowercaseCodes).

/* Predicato che converte un singolo carattere in minuscolo. */
to_lower(Code, LowerCode) :-
    Code >= 65, Code =< 90,
    LowerCode is Code + 32.
to_lower(Code, Code).

/* Predicato che restituisce il peso associato ad un genere. Se non è
   stato definito un peso specifico, viene utilizzato un peso predefinito (1). */
peso_genere(Genere, Peso) :- 
    (   genere_preferito(Genere, Peso) -> true ; Peso = 1 ).

/* Predicato che rimuove tutti gli spazi da una stringa, sia che sia
   rappresentata come atomo o come lista di codici ASCII. */
rimuovi_spazi(Stringa, StringaRimossa) :-
    (    atom(Stringa) -> atom_codes(Stringa, Codici) ; Codici = Stringa ),
    rimuovi_spazi_codici(Codici, CodiciRimossi),
    (    atom(Stringa) -> atom_codes(StringaRimossa, CodiciRimossi) ; StringaRimossa = CodiciRimossi ).

/* Predicato ausiliario che rimuove gli spazi (codice ASCII 32)
   da una lista di codici ASCII. */
rimuovi_spazi_codici([], []).
rimuovi_spazi_codici([32|T], R) :- rimuovi_spazi_codici(T, R).
rimuovi_spazi_codici([H|T], [H|R]) :- H \= 32, rimuovi_spazi_codici(T, R).