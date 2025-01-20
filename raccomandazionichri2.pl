/* ######################################################### */
/* # Corso di Programmazione Logica e Funzionale           # */
/* # Progetto di raccomandazione di canzoni                # */
/* # Studente: Giaconi Christian, Giacomo Rossi            # */
/* # Matricola: 314045, 314671                             # */
/* ######################################################### */


/* ================================================
   Predicati dinamici
   ================================================ */
:- dynamic(canzone/4).
:- dynamic(genere_preferito/2).
:- dynamic(stringa/1).

/* ================================================
   Predicati principali
   ================================================ */
main :-
    write('Benvenuto nel sistema di raccomandazione musicale!\n'),
    loop_menu.

loop_menu :-
    write('\nScegli un\'azione:\n'),
    write('1. Carica un file con le canzoni\n'),
    write('2. Gestisci i generi preferiti (aggiungi o modifica)\n'),
    write('3. Stampa la classifica delle canzoni\n'),
    write('4. Stampa la lista dei generi preferiti\n'),
    write('5. Esci\n'),
    read(Scelta),
    (   Scelta = 1 -> carica_canzoni
    ;   Scelta = 2 -> gestisci_generi_preferiti
    ;   Scelta = 3 -> stampa_classifica
    ;   Scelta = 4 -> mostra_generi_preferiti
    ;   Scelta = 5 -> write('Arrivederci!\n'), halt
    ;   write('Scelta non valida. Riprova.\n')
    ),
    loop_menu.

/* Carica canzoni da file */
carica_canzoni :-
    write('Inserisci il nome del file: '),
    read(File),
    open(File, read, Stream),
    read_canzoni_da_file(Stream),
    close(Stream).

/* Legge canzoni da un file */
read_canzoni_da_file(Stream) :-
    read(Stream, canzone(Titolo, Artista, Genere, Punteggio)),
    (   Titolo == end_of_file
    ->  true
    ;   assertz(canzone(Titolo, Artista, Genere, Punteggio)),
        read_canzoni_da_file(Stream)
    ).

/* ================================================
   Predicati per la raccomandazione e la classifica
   ================================================ */
stampa_canzoni_ordinate([], _).
stampa_canzoni_ordinate([PunteggioPonderato-Titolo | Rest], Posizione) :- 
    canzone(Titolo, Artista, Genere, _),
    format('~d# ~w (Artista: ~w, Genere: ~w, Punteggio ponderato: ~2f)\n', 
           [Posizione, Titolo, Artista, Genere, PunteggioPonderato]),
    stampa_canzoni_ordinate(Rest, Posizione + 1).

/* Predicato che calcola il punteggio ponderato per ogni canzone 
   in base al suo genere e al suo punteggio originale. */
stampa_classifica :- 
    findall(PunteggioPonderato-Titolo, calcola_punteggio_ponderato(Titolo, PunteggioPonderato), Punteggi),
    (   Punteggi == []
    ->  write('Nessuna canzone trovata con punteggio ponderato.\n')
    ;   list_to_set(Punteggi, PunteggiUnici),
        sort_descending(PunteggiUnici, PunteggiOrdinati),
        write('\n'),
        stampa_canzoni_ordinate(PunteggiOrdinati, 1)
    ).

/* Predicato che calcola il punteggio ponderato
   di una canzone in base al suo genere (e al peso preferito associato)
   moltiplicato per il punteggio originale della canzone. */
calcola_punteggio_ponderato(Titolo, PunteggioPonderato) :- 
    canzone(Titolo, _, Genere, Punteggio),
    normalizza_genere(Genere, GenereNormalizzato),
    peso_genere(GenereNormalizzato, Peso),
    PunteggioPonderato is Punteggio * Peso.

/* ================================================
   Predicati ausiliari
   ================================================ */
sort_descending([], []).
sort_descending([X], [X]).
sort_descending([X, Y | Rest], [X | SortedRest]) :- 
    compare(>, X, Y), !, 
    sort_descending([Y | Rest], SortedRest).
sort_descending([X, Y | Rest], [Y | SortedRest]) :- 
    sort_descending([X | Rest], SortedRest).

peso_genere(Genere, Peso) :- 
    (   genere_preferito(Genere, Peso) -> true ; Peso = 1 ).

/* ================================================
   Predicati per la gestione dei generi preferiti
   ================================================ */
gestisci_generi_preferiti :- 
    mostra_generi_disponibili,
    write('Inserisci i tuoi generi preferiti, uno per volta. Scrivi "fine" per terminare.\n'),
    chiedi_generi_preferiti([]).

chiedi_generi_preferiti(GeneriPreferiti) :- 
    ottieni_generi_disponibili(GeneriDisponibili),
    write('Inserisci un genere preferito: '),
    read(Genere),
    (   Genere == fine
    ->  chiedi_peso_generi(GeneriPreferiti)
    ;   normalizza_genere(Genere, GenereNormalizzato),
        downcase_atom(GenereNormalizzato, GenereNormalizzatoMinuscolo),
        maplist(downcase_atom, GeneriDisponibili, GeneriDisponibiliMinuscolo),
        (   member(GenereNormalizzatoMinuscolo, GeneriDisponibiliMinuscolo)
        ->  append(GeneriPreferiti, [GenereNormalizzato], NuoviGeneri),
            chiedi_generi_preferiti(NuoviGeneri)
        ;   write('Genere non valido. Riprova.\n'),
            chiedi_generi_preferiti(GeneriPreferiti)
        )
    ).

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
   Predicati ausiliari per la gestione delle stringhe
   ================================================ */
normalizza_genere(Genere, GenereNormalizzato) :-
    (atom(Genere) -> true ; atom_codes(Genere, Genere)),
    downcase_atom(Genere, GenereLower),
    atom_codes(GenereLower, Codici),
    rimuovi_spazi(Codici, CodiciNormalizzati),
    atom_codes(GenereNormalizzato, CodiciNormalizzati).

rimuovi_spazi(Stringa, StringaRimossa) :-
    (    atom(Stringa) -> atom_codes(Stringa, Codici) ; Codici = Stringa ),
    rimuovi_spazi_codici(Codici, CodiciRimossi),
    (    atom(Stringa) -> atom_codes(StringaRimossa, CodiciRimossi) ; StringaRimossa = CodiciRimossi ).

rimuovi_spazi_codici([], []).
rimuovi_spazi_codici([32|T], R) :- rimuovi_spazi_codici(T, R).
rimuovi_spazi_codici([H|T], [H|R]) :- H \= 32, rimuovi_spazi_codici(T, R).

/* Predicato che restituisce i generi disponibili */
ottieni_generi_disponibili(GeneriDisponibili):-
    findall(Genere, canzone(_, _, Genere, _), Generi),
    sort(Generi, GeneriOrdinati),
    GeneriDisponibili = GeneriOrdinati.

mostra_generi_disponibili :-
    ottieni_generi_disponibili(GeneriOrdinati),
    format('Generi disponibili: ~w\n', [GeneriOrdinati]).

/* Predicato che mostra i generi preferiti associati
   con il rispettivo peso. */
mostra_generi_preferiti :- 
    findall(Genere-Peso, genere_preferito(Genere, Peso), Generi),
    (   Generi == []
    ->  write('Non è stato definito alcun genere preferito.\n')
    ;   write('I tuoi generi preferiti e i loro pesi:\n'),
        stampa_generi(Generi)
    ).

stampa_generi([]).
stampa_generi([Genere-Peso | Rest]) :- 
    format('~w: ~w\n', [Genere, Peso]),
    stampa_generi(Rest).

downcase_atom(Atom, DowncasedAtom) :-
    atom_chars(Atom, Chars),
    maplist(downcase_char, Chars, DowncasedChars),
    atom_chars(DowncasedAtom, DowncasedChars).

downcase_char(Char, DowncasedChar) :-
    char_type(Char, upper(LowerChar)),
    DowncasedChar = LowerChar.
downcase_char(Char, Char).

list_to_set([], []).
list_to_set([H|T], [H|SetT]) :-
    \+ member(H, T),
    list_to_set(T, SetT).
list_to_set([H|T], Set) :-
    member(H, T),
    list_to_set(T, Set).

char_type(Char, upper(Char)) :- char_code(Char, Code), Code >= 65, Code =< 90.
char_type(Char, lower(Char)) :- char_code(Char, Code), Code >= 97, Code =< 122.
