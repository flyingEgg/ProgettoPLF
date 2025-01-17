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

    :- dynamic(stringa/1).



   
   
   /* ================================================
      Predicati principali
      ================================================ */
   
   /* Predicato che 'main' è il punto di ingresso principale.
      Inizializza il programma e avvia il menu interattivo
      per l'utente. */
   main :- 
       write('Benvenuto nel sistema di raccomandazione musicale!\n'),
       loop_menu.
   
   /* Predicato che 'loop_menu' gestisce la selezione delle azioni
      da parte dell'utente nel menu principale. Ogni opzione del menu
      chiama un predicato specifico per eseguire l'azione corrispondente. */
   loop_menu :- 
       write('\nScegli un\'azione:\n'),
       write('1. Carica un file con le canzoni\n'),
       write('2. Gestisci i generi preferiti (aggiungi o modifica)\n'),
       write('3. Stampa la classifica delle canzoni\n'),
       write('4. Stampa la lista dei generi preferiti\n'),
       write('5. Esci\n'),
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
      Predicati di caricamento delle canzoni
      ================================================ */
   
   /* Predicato che permette all'utente di inserire il nome del
      file che contiene le canzoni.
      Se il caricamento ha successo, stampa un messaggio di conferma. */
   carica_canzoni_interattivo :- 
       write('Inserire tra apici il nome del file contenente le canzoni: '), nl,
       read(File),
       (   catch(carica_canzoni(File), _, fail)
       ->  write('\n\nCanzoni caricate con successo!\n'),
           mostra_generi_disponibili
       ;   write('\nErrore nel caricamento del file. Riprova.\n')
       ).
   

% boh riprendi da qui
   carica_canzoni(File) :-
       open(File, read, Stream),
       leggi_righe(Stream, Lines),
       close(Stream),
       processa_righe(Lines).

   leggi_righe(Stream,[]):-
       at_end_of_stream(Stream).
   leggi_righe(Stream,[X|L]):-
       \+ at_end_of_stream(Stream),
       get_char(Stream,X),
       leggi_righe(Stream,L).

   processa_righe([]) :- !.
   processa_righe([Char | Rest]) :-
       processa_riga_singola(Rest, [Char], Stringa, RestDopoLinea),
       write(Stringa),
       write('\n'),
       parsing_righe(Stringa),
       processa_righe(RestDopoLinea).

   processa_riga_singola([], Accumulatore, Stringa, []) :-
        atom_chars(Stringa, Accumulatore).

   processa_riga_singola(['\n' | Rest], Accumulatore, Stringa, Rest) :-
        atom_chars(Stringa, Accumulatore).

   processa_riga_singola([Char | Rest], Accumulatore, Stringa, RestDopoLinea) :-
        Char \= '\n',
        append(Accumulatore, [Char], NuovoAccumulatore),
        processa_riga_singola(Rest, NuovoAccumulatore, Stringa, RestDopoLinea).


   parsing_righe(Riga) :-
       write('\nEntro nel parsing...'),
       split_string(Riga, ',', "", [Titolo, Artista, Genere, PunteggioStr]),
       write('\nSplit string effettuato...'),
       number_string(Punteggio, PunteggioStr),
       write('\nNumber string effettuato...'),
       assertz(canzone(Titolo, Artista, Genere, Punteggio)),
       write('\nAssertz effettuato...'),
       format('~w (Artista: ~w, Genere: ~w, Punteggio ponderato: ~2f)\n',
               [Titolo, Artista, Genere, Punteggio]).

    /* ================================================
       Predicati ausiliari per la gestione delle stringhe
       ================================================ */

       % Definisci il predicato number_string/2
    number_string(Number, String) :-
        var(Number), !,
        atom_codes(String, Codes),
        number_codes(Number, Codes).    % e' lui il problema

    number_string(Number, String) :-
        number(Number), !,
        number_codes(Number, Codes),
        atom_codes(String, Codes).


       % Definisci il predicato split_string/4
    split_string(String, Separator, Padding, Substrings) :-
        atom_codes(String, StringCodes),
        atom_codes(Separator, SeparatorCodes),
        atom_codes(Padding, PaddingCodes),
        split_string_codes(StringCodes, SeparatorCodes, PaddingCodes, Substrings).

    split_string_codes([], _, _, []) :- !.
    split_string_codes(StringCodes, SeparatorCodes, PaddingCodes, [Substring|Substrings]) :-
        split_string_codes_aux(StringCodes, SeparatorCodes, PaddingCodes, SubstringCodes, RestCodes),
        atom_codes(Substring, SubstringCodes),
        split_string_codes(RestCodes, SeparatorCodes, PaddingCodes, Substrings).

    split_string_codes_aux([], _, _, [], []) :- !.
    split_string_codes_aux([C|Cs], SeparatorCodes, PaddingCodes, [], Cs) :-
        member(C, SeparatorCodes), !.
    split_string_codes_aux([C|Cs], SeparatorCodes, PaddingCodes, [C|SubstringCodes], RestCodes) :-
        \+ member(C, SeparatorCodes),
        \+ member(C, PaddingCodes), !,
        split_string_codes_aux(Cs, SeparatorCodes, PaddingCodes, SubstringCodes, RestCodes).
    split_string_codes_aux([C|Cs], SeparatorCodes, PaddingCodes, SubstringCodes, RestCodes) :-
        member(C, PaddingCodes), !,
        split_string_codes_aux(Cs, SeparatorCodes, PaddingCodes, SubstringCodes, RestCodes).




/*
    leggi_canzoni(Stream) :-
            read_line_to_codes(Stream, Codici),
            (   Codici \= end_of_file
            ->  string_codes(Linia, Codici),
                split_string(Linia, ",", " ", [Titolo, Artista, Genere, PunteggioStr]),
                number_string(Punteggio, PunteggioStr),
                (   \+ canzone(Titolo, Artista, Genere, Punteggio)
                ->  assertz(canzone(Titolo, Artista, Genere, Punteggio))
                ;   true
                ),
                leggi_canzoni(Stream)
            ;   true ).
   */

   
   /* ================================================
      Predicati per la gestione dei generi preferiti
      ================================================ */
   
   /* Predicato che permette all'utente di selezionare 
      e gestire i generi musicali preferiti. */
   gestisci_generi_preferiti :- 
       mostra_generi_disponibili,
       write('Inserisci i tuoi generi preferiti, uno per volta. Scrivi "fine" per terminare.\n'),
       chiedi_generi_preferiti([]).
   
   /* Predicato che raccoglie i generi preferiti inseriti
      dall'utente e li aggiunge alla lista di preferiti. */
   chiedi_generi_preferiti(GeneriPreferiti) :- 
       write('Inserisci un genere preferito: '),
       read(Genere),
       (   Genere == fine
       ->  chiedi_peso_generi(GeneriPreferiti)
       ;   normalizza_genere(Genere, GenereNormalizzato),
           append(GeneriPreferiti, [GenereNormalizzato], NuoviGeneri),
           chiedi_generi_preferiti(NuoviGeneri) ).
   
   /* Predicato che chiede all'utente di inserire
      un peso per ciascun genere musicale preferito. */
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
       findall(PunteggioPonderato-Titolo, calcola_punteggio_ponderato(Titolo, PunteggioPonderato), Punteggi),
       (   Punteggi == []
       ->  write('Nessuna canzone trovata con punteggio ponderato.\n')
       ;   maplist(invert_punteggio, Punteggi, InvertedPunteggi),
           keysort(InvertedPunteggi, SortedInverted),
           maplist(invert_punteggio, SortedInverted, Ordinata),
           stampa_canzoni_ordinate(Ordinata, 1)
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
   
      atom_number(Atom, Number) :-
       atom_codes(Atom, Codes),
       number_codes(Number, Codes).
   
   
   read_line(Stream, Line) :-
       leggi_riga_acc(Stream, [], Line).
   
   leggi_riga_acc(Stream, Acc, Line) :-
       get_char(Stream, Char),
       (   Char == end_of_file
       ->  Line = end_of_file
       ;   Char == '\n'
       ->  reverse(Acc, Line)
       ;   leggi_riga_acc(Stream, [Char | Acc], Line)
       ).
   
   read_chars(Stream, Char, Line) :-
       (   Char == '\n'
       ->  Line = []
       ;   get_char(Stream, NextChar),
           read_chars(Stream, NextChar, Rest),
           Line = [Char | Rest]
       ).
   
   split_string(Input, Sep, Parts) :-
       atom_codes(Input, Codes),
       atom_codes(Sep, [SepCode]),
       split_codes(Codes, SepCode, [], Parts).
       
   
       split_codes([], _, Acc, [Part]) :-
           atom_codes(Part, Acc).
       split_codes([Sep|Rest], Sep, Acc, [Part|Parts]) :-
           atom_codes(Part, Acc),
           split_codes(Rest, Sep, [], Parts).
       split_codes([C|Rest], Sep, Acc, Parts) :-
           C \= Sep,
           append(Acc, [C], NewAcc),
           split_codes(Rest, Sep, NewAcc, Parts).
   
   /* Predicato che mostra i generi preferiti associati
      con il rispettivo peso. */
   mostra_generi_preferiti :- 
       findall(Genere-Peso, genere_preferito(Genere, Peso), Generi),
       (   Generi == []
       ->  write('Non è stato definito alcun genere preferito.\n')
       ;   write('I tuoi generi preferiti e i loro pesi:\n'),
           stampa_generi(Generi)
       ).
   
   /* Predicato che restituisce una lista dei generi musicali 
      univoci presenti nel database delle canzoni. */
   mostra_generi_disponibili :- 
       findall(Genere, canzone(_, _, Genere, _), Generi),
       rimuovi_duplicati(Generi, GeneriSenzaDuplicati),
       ordina_generi(GeneriSenzaDuplicati, GeneriOrdinati),
       format('Generi disponibili: ~w\n', [GeneriOrdinati]).
   
   /* Predicato che rimuove i duplicati da una lista */
   rimuovi_duplicati([], []).
   rimuovi_duplicati([H|T], [H|Rest]) :-
       \+ member(H, T),
       rimuovi_duplicati(T, Rest).
   rimuovi_duplicati([H|T], Rest) :-
       member(H, T),
       rimuovi_duplicati(T, Rest).
   
   /* Ordina i generi */
   ordina_generi([], []).
   ordina_generi([Genere], [Genere]).
   ordina_generi([Genere1, Genere2 | Rest], [Genere1 | RestOrdinato]) :-
       ordina_generi([Genere2 | Rest], RestOrdinato),
       ordina_lista(Genere1, RestOrdinato).
   
   /* Predicato ausiliario per ordinare un genere */
   ordina_lista([], []).
   ordina_lista([H|T], [H|SortedT]) :-
       ordina_lista(T, SortedT),
       H @=< SortedT.
   ordina_lista([H|T], [SortedH|SortedT]) :-
       ordina_lista(T, SortedT),
       H @> SortedH.
   
   
   /* Predicato che stampa la lista dei generi preferiti. */
   stampa_generi([]).
   stampa_generi([Genere-Peso | Rest]) :- 
       format('~w: ~w\n', [Genere, Peso]),
       stampa_generi(Rest).
   
   /* Predicato che normalizza il genere, 
      trasformandolo in minuscolo e rimuovendo eventuali spazi. */
   normalizza_genere(Genere, GenereNormalizzato) :- 
       downcase_atom(Genere, GenereLower),
       atom_codes(GenereLower, Codici),
       rimuovi_spazi(Codici, CodiciNormalizzati),
       atom_codes(GenereNormalizzato, CodiciNormalizzati).
   
   downcase_atom(Atom, LowercaseAtom) :-
       atom_codes(Atom, Codes),
       maplist(to_lower, Codes, LowercaseCodes),
       atom_codes(LowercaseAtom, LowercaseCodes).
       
   to_lower(Code, LowerCode) :-
       Code >= 65, Code =< 90,
       LowerCode is Code + 32.
   to_lower(Code, Code).
   
   sort_descending(List, Sorted) :-
       maplist(invert_punteggio, List, InvertedList),
       keysort(InvertedList, SortedInverted),
       maplist(invert_punteggio, SortedInverted, Sorted).
   
   invert_punteggio(-Punteggio, Titolo, Punteggio-Titolo) :- !.
   invert_punteggio(Punteggio, Titolo, -Punteggio-Titolo).
   
   compare_descending(Delta, X, Y) :-
       compare(DeltaReverse, X, Y),
       reverse_compare(DeltaReverse, Delta).
   
   reverse_compare(<, >).
   reverse_compare(=, =).
   reverse_compare(>, <).
   
   /* Predicato che 'peso_genere' restituisce il peso di un genere.
      Se non è specificato, viene utilizzato un peso di 1. */
   peso_genere(Genere, Peso) :- 
       (   genere_preferito(Genere, Peso) -> true ; Peso = 1 ).
   
   /* Predicato che rimuove gli spazi da una stringa. */
   rimuovi_spazi(Stringa, StringaRimossa) :- 
       atom_codes(Stringa, Codici),
       rimuovi_spazi_codici(Codici, CodiciRimossi),
       atom_codes(StringaRimossa, CodiciRimossi).
   
   /* Predicato che rimuove gli spazi dalla lista
      di codici ASCII di una stringa. */
   rimuovi_spazi_codici([], []).
   rimuovi_spazi_codici([32|T], R) :- rimuovi_spazi_codici(T, R).
   rimuovi_spazi_codici([H|T], [H|R]) :- H \= 32, rimuovi_spazi_codici(T, R).