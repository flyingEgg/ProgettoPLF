/* ######################################################### */
/* # Corso di Programmazione Logica e Funzionale           # */
/* # Progetto di raccomandazione di canzoni                # */
/* # Studente: Giaconi Christian, Giacomo Rossi            # */
/* # Matricola: 314045, 314671                             # */
/* ######################################################### */

/* Specifica:
    Scrivere un programma in Prolog per implementare un sistema avanzato di raccomandazione di canzoni.
    Il sistema suggerisce canzoni a un utente in base a:
    - Preferenze per uno o più generi musicali specificati.
    - Un sistema di punteggio ponderato per dare priorità a canzoni più rilevanti.
    L'utente deve fornire un file di testo con le canzoni nel seguente formato:
        Titolo,Artista,Genere,Punteggio
    Dove "Punteggio" è un intero da 1 a 10.
    Le canzoni saranno ordinate in base al punteggio ponderato e filtrate per genere.
*/

/* ================================================
   Dichiarazioni dinamiche dei predicati
   ================================================ */
:- dynamic canzone/4.
:- dynamic genere_preferito/2.
:- dynamic punteggio_ponderato/2.

/* ================================================
   Caricamento dinamico delle canzoni da file
   ================================================ */

/* Carica le canzoni da un file di testo evitando duplicati in base a Titolo, Artista e Genere */
carica_canzoni(File) :- 
    open(File, read, Stream),
    leggi_canzoni(Stream),
    close(Stream).

/* Legge il file riga per riga */
leggi_canzoni(Stream) :- 
    read_line_to_string(Stream, Riga), 
    (   Riga \= end_of_file
    ->  split_string(Riga, ",", " ", [Titolo, Artista, Genere, PunteggioStr]),
        (   number_string(Punteggio, PunteggioStr)
        ->  (   \+ canzone(Titolo, Artista, Genere, _)  % Controlla se la canzone non esiste già (ignorando il punteggio)
            ->  assertz(canzone(Titolo, Artista, Genere, Punteggio))
            ;   format('La canzone ~w di ~w del genere ~w e' gia' presente nel database.~n', [Titolo, Artista, Genere])
            )
        ;   format('Errore nel formato della riga: ~w. Riga ignorata.~n', [Riga])
        ),
        leggi_canzoni(Stream)
    ;   true
    ).

/* ================================================
   Calcolo dei punteggi ponderati
   ================================================ */

/* Calcola il punteggio ponderato di una canzone, basato sul genere preferito. */
calcola_punteggio_ponderato(Titolo, PunteggioPonderato) :-
    canzone(Titolo, _, Genere, Punteggio),
    peso_genere(Genere, Peso),  % Recupera il peso associato al genere
    PunteggioPonderato is Punteggio * Peso,
    format('DEBUG: Canzone ~w, Genere ~w, Punteggio ~w, Peso ~2f, Punteggio ponderato ~2f~n',
           [Titolo, Genere, Punteggio, Peso, PunteggioPonderato]).

/* Gestione del peso per generi definiti e predefiniti */
peso_genere(Genere, Peso) :-
    (genere_preferito(Genere, Peso)
    ->  format('DEBUG: Genere preferito ~w trovato con peso ~2f~n', [Genere, Peso])
    ;   Peso = 1,  % Se il genere non è preferito, assegna peso 1
        format('DEBUG: Genere ~w non trovato, peso predefinito 1~n', [Genere])).

  
/* Definisce i pesi per i generi musicali preferiti. */
genere_preferito('Bachata', 1.7).       /* Peso maggiore per la Bachata */
genere_preferito('Merengue', 1.2).      /* Peso maggiore per il Merengue */

/* ================================================
   Aggiornamento dei punteggi ponderati
   ================================================ */

aggiorna_punteggi :- 
    retractall(punteggio_ponderato(_, _)),  % Rimuove tutti i vecchi punteggi ponderati
    forall(canzone(Titolo, _, Genere, Punteggio), 
           (peso_genere(Genere, Peso), 
            PunteggioPonderato is Punteggio * Peso,
            format('DEBUG: Canzone ~w, Genere ~w, Punteggio ~w, Peso ~w, Punteggio ponderato ~2f~n',
                   [Titolo, Genere, Punteggio, Peso, PunteggioPonderato]),
            assertz(punteggio_ponderato(Titolo, PunteggioPonderato)),
            format('Canzone: ~w, Punteggio ponderato aggiornato: ~2f~n', [Titolo, PunteggioPonderato]))).

/* ================================================
   Predicati per la gestione e ordinamento delle canzoni
   ================================================ */

/* Stampa la classifica ordinata */
stampa_classifica :- 
    aggiorna_punteggi,  % Aggiorna i punteggi ponderati
    findall(PunteggioPonderato-Titolo, 
            punteggio_ponderato(Titolo, PunteggioPonderato), 
            Punteggi),
    sort(1, @>=, Punteggi, Ordinata),  % Ordina per punteggio ponderato decrescente
    stampa_canzoni_ordinate(Ordinata, 1).

/* Stampa le canzoni ordinate in base al punteggio ponderato */
stampa_canzoni_ordinate([], _).
stampa_canzoni_ordinate([PunteggioPonderato-Titolo | Rest], Posizione) :-
    canzone(Titolo, Artista, Genere, _),  % Recupera le altre informazioni dalla base di dati
    format('~d# Canzone: ~w~n   Artista: ~w~n   Genere: ~w~n   Punteggio ponderato: ~2f~n~n',
           [Posizione, Titolo, Artista, Genere, PunteggioPonderato]),
    NuovaPosizione is Posizione + 1,
    stampa_canzoni_ordinate(Rest, NuovaPosizione).

/* Classifica filtrata per genere */
classifica_per_genere(Genere) :-
    % Seleziona le canzoni del genere richiesto
    findall(Titolo, canzone(Titolo, _, Genere, _), CanzoniFiltrate),
    % Calcola i punteggi per le canzoni filtrate
    findall(PunteggioPonderato-Titolo, 
            (member(Titolo, CanzoniFiltrate), 
             calcola_punteggio_ponderato(Titolo, PunteggioPonderato)), 
            Punteggi),
    % Ordina per punteggio ponderato decrescente
    sort(1, @>=, Punteggi, Ordinata),
    % Stampa le canzoni ordinate
    stampa_canzoni_ordinate(Ordinata, 1).

mostra_canzoni_genere(Genere) :-
    findall(Titolo, canzone(Titolo, _, Genere, _), CanzoniFiltrate),
    format('Canzoni di genere ~w: ~w~n', [Genere, CanzoniFiltrate]).

/* Stampa i generi preferiti */
stampa_generi_preferiti :-
    write('Generi musicali preferiti e i loro pesi:'), nl,
    findall((Genere, Peso), 
            genere_preferito(Genere, Peso), 
            Generi),
    forall(member((Genere, Peso), Generi),
           format('Genere: ~w, Peso: ~2f~n', [Genere, Peso])).

/* Salva la classifica ordinata su file */
salva_classifica(File) :-
    aggiorna_punteggi,
    findall(Punteggio-Titolo, 
            punteggio_ponderato(Titolo, Punteggio), 
            Punteggi),
    sort(1, @>=, Punteggi, Ordinata),
    open(File, write, Stream),
    forall(member(Punteggio-Titolo, Ordinata),
           (canzone(Titolo, Artista, Genere, _),
            format(Stream, '~w, ~w, ~w, ~2f~n', [Titolo, Artista, Genere, Punteggio]))),
    close(Stream).

/* ================================================
   Aggiunta o rimozione di un genere preferito
   ================================================ */

aggiungi_genere_preferito(Genere, Peso) :-
    Peso > 0,
    retractall(genere_preferito(Genere, _)),
    assertz(genere_preferito(Genere, Peso)),
    aggiorna_punteggi.

rimuovi_genere_preferito(Genere) :-
    retractall(genere_preferito(Genere, _)),
    aggiorna_punteggi.

/* ================================================
   Funzione principale per avviare il programma
   ================================================ */

/* Main per interazione con l'utente */
main :- 
    write('Benvenuto nel sistema di raccomandazione musicale!~n'),
    write('Per favore, carica il file delle canzoni.~n'),
    read(File),  % L'utente inserisce il nome del file
    carica_canzoni(File),
    write('Inserisci i tuoi generi musicali preferiti e il loro peso.~n'),
    chiedi_preferenze,  % Funzione per chiedere i generi e i pesi
    write('Classifica delle canzoni ordinata per punteggio ponderato:~n'),
    stampa_classifica.  % Stampa la classifica ordinata

/* Funzione per chiedere i generi musicali preferiti e il loro peso */
chiedi_preferenze :-
    write('Inserisci un genere preferito (oppure fine per terminare): '),
    read(Genere),
    (   Genere \= fine
    ->  write('Inserisci il peso per il genere ~w: ', [Genere]),
        read(Peso),
        aggiungi_genere_preferito(Genere, Peso),
        chiedi_preferenze  % Chiede per altri generi
    ;   true  % Termina quando l'utente inserisce "fine"
    ).
