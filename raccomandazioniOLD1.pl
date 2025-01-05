/* ######################################################### */
/* # Corso di Programmazione Logica e Funzionale           # */
/* # Progetto di raccomandazione di canzoni                # */
/* # Studente: Giaconi Christian, Giacomo Rossi            # */
/* # Matricola: 314045, 314671                             # */
/* ######################################################### */

:- dynamic canzone/4.
:- dynamic genere_preferito/2.

/* ================================================
   Funzione principale
   ================================================ */
main :- 
    write('Benvenuto nel sistema di raccomandazione musicale!\n'),
    carica_file_canzoni,
    imposta_preferenze,
    stampa_classifica.

/* Loop per il caricamento del file*/
carica_file_canzoni :-
    repeat,
    write('Inserisci il nome del file contenente le canzoni: '),
    read(File),
    (   carica_canzoni(File)
    ->  write('Canzoni caricate con successo!\n'), !
    ;   write('Errore nel caricamento del file. Riprova.\n'), fail ).

/* Loop per l'impostazione delle preferenze */
imposta_preferenze :- 
    trova_generi_disponibili(Generi),
    format('Generi disponibili nel file: ~w\n', [Generi]),
    write('Inserisci i tuoi generi preferiti, uno per volta. Inserisci "fine" per terminare: '), nl,
    chiedi_generi_preferiti([]).

/* Chiede i generi preferiti, uno alla volta */
chiedi_generi_preferiti(GeneriPreferiti) :-
    write('Inserisci un genere preferito: '),
    read(Genere),
    normalize_genere(Genere, NormalizedGenere),  % Normalize user input
    (   Genere == fine
    ->  format('Generi preferiti separati: ~w\n', [GeneriPreferiti]),
        chiedi_peso_generi(GeneriPreferiti)
    ;   (   member(NormalizedGenere, GeneriPreferiti)
        ->  write('Hai già inserito questo genere. Inserisci un altro genere o "fine" per terminare.\n'),
            chiedi_generi_preferiti(GeneriPreferiti)
        ;   append(GeneriPreferiti, [NormalizedGenere], NuoviGeneri),
            chiedi_generi_preferiti(NuoviGeneri)
    )).

/* Chiede il peso per ciascun genere preferito */
chiedi_peso_generi([]).
chiedi_peso_generi([Genere | Altri]) :-
    format('Inserisci il peso per il genere ~w: ', [Genere]),
    read(Peso),
    (   number(Peso), Peso > 0
    ->  assertz(genere_preferito(Genere, Peso)),
        chiedi_peso_generi(Altri)
    ;   write('Peso non valido. Riprova.\n'),
        chiedi_peso_generi([Genere | Altri]) ).

/* ================================================
   Caricamento dinamico delle canzoni da file
   ================================================ */
carica_canzoni(File) :-
    open(File, read, Stream),
    leggi_canzoni(Stream),
    close(Stream).

leggi_canzoni(Stream) :-
    read_line_to_codes(Stream, Codes),
    (   Codes \= end_of_file
    ->  string_codes(Line, Codes),
        split_string(Line, ",", " ", [Titolo, Artista, Genere, PunteggioStr]),
        number_string(Punteggio, PunteggioStr),
        (   \+ canzone(Titolo, Artista, Genere, Punteggio) % Check if song is already present
        ->  assertz(canzone(Titolo, Artista, Genere, Punteggio))
        ;   true 
        ),
        leggi_canzoni(Stream)
    ;   true ).

trova_generi_disponibili(Generi) :-
    findall(Genere, canzone(_, _, Genere, _), GeneriDuplicati),
    sort(GeneriDuplicati, Generi).

/* ================================================
   Calcolo e stampa della classifica
   ================================================ */
stampa_classifica :-
    write('Classifica delle canzoni in base al punteggio ponderato:\n'),
    findall(PunteggioPonderato-Titolo, calcola_punteggio_ponderato(Titolo, PunteggioPonderato), Punteggi),
    (   Punteggi == []
    ->  write('Nessuna canzone trovata con punteggio ponderato.\n')
    ;   sort(1, @>=, Punteggi, Ordinata),
        stampa_canzoni_ordinate(Ordinata, 1)
    ).

calcola_punteggio_ponderato(Titolo, PunteggioPonderato) :-
    canzone(Titolo, _, Genere, Punteggio),
    normalize_genere(Genere, NormalizedGenere),
    peso_genere(NormalizedGenere, Peso), 
    PunteggioPonderato is Punteggio * Peso.

trim(String, Trimmed) :-
    string_codes(String, Codes),
    trim_codes(Codes, TrimmedCodes),
    string_codes(Trimmed, TrimmedCodes).

trim_codes([], []).
trim_codes([32|T], Trimmed) :- 
    trim_codes(T, Trimmed).
trim_codes([32], []).
trim_codes([H|T], [H|Trimmed]) :- 
    H \= 32,
    trim_codes(T, Trimmed).

normalize_genere(Genere, NormalizedGenere) :-
    string_lower(Genere, LowercaseGenere),
    trim(LowercaseGenere, NormalizedGenere).

peso_genere(Genere, Peso) :-
    (   genere_preferito(Genere, Peso)
    ->  true
    ;   Peso = 1 ).

stampa_canzoni_ordinate([], _).
stampa_canzoni_ordinate([PunteggioPonderato-Titolo | Rest], Posizione) :-
    canzone(Titolo, Artista, Genere, _),
    format('~d# ~w (Artista: ~w, Genere: ~w, Punteggio ponderato: ~2f)\n', 
           [Posizione, Titolo, Artista, Genere, PunteggioPonderato]),
    NuovaPosizione is Posizione + 1,
    stampa_canzoni_ordinate(Rest, NuovaPosizione).