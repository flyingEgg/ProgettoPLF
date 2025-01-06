/* ########################################### */
/* # Corso di Programmazione Logica e Funzionale */
/* # Progetto di raccomandazione di canzoni    */
/* ########################################### */

/* ================================================ 
   Predicati dinamici 
   ================================================ */

:- dynamic(canzone/4).
:- dynamic(genere_preferito/2).

carica_canzoni :- ensure_loaded('canzoni.pl').

/* Gestione dei generi preferiti */
gestisci_generi_preferiti :- 
    mostra_generi_disponibili,
    write('Inserisci i tuoi generi preferiti, uno per volta. Scrivi "fine" per terminare.\n'),
    chiedi_generi_preferiti([]).

chiedi_generi_preferiti(GeneriPreferiti) :- 
    write('Inserisci un genere preferito: '),
    read(Genere),
    (   Genere == fine
    ->  chiedi_peso_generi(GeneriPreferiti)
    ;   % Verifica se il genere è valido
        genera_disponibili(GeneriDisponibili),
        (   member(Genere, GeneriDisponibili)
        ->  append(GeneriPreferiti, [Genere], NuoviGeneri),
            chiedi_generi_preferiti(NuoviGeneri)
        ;   write('Genere non valido. Riprova.\n'),
            chiedi_generi_preferiti(GeneriPreferiti)
        ) ).

genera_disponibili(GeneriDisponibili) :- 
    findall(Genere, canzone(_, _, Genere, _), Generi),
    sort(Generi, GeneriDisponibili).

chiedi_peso_generi([]).
chiedi_peso_generi([Genere | Altri]) :- 
    format('Inserisci il peso per il genere ~w: ', [Genere]),
    read(Peso),
    (   number(Peso), Peso > 0
    ->  assertz(genere_preferito(Genere, Peso)),
        chiedi_peso_generi(Altri)
    ;   write('Peso non valido. Riprova.\n'),
        chiedi_peso_generi([Genere | Altri]) ).

/* Predicati per la raccomandazione e la classifica */
stampa_classifica :- 
    findall(PunteggioPonderato-Titolo, calcola_punteggio_ponderato(Titolo, PunteggioPonderato), Punteggi),
    (   Punteggi == []
    ->  write('Nessuna canzone trovata con punteggio ponderato.\n')
    ;   ordina_punteggi(Punteggi, Ordinata),
        stampa_canzoni_ordinate(Ordinata, 1)
    ).

calcola_punteggio_ponderato(Titolo, PunteggioPonderato) :- 
    canzone(Titolo, _, Genere, Punteggio),
    peso_genere(Genere, Peso),
    PunteggioPonderato is Punteggio * Peso.

stampa_canzoni_ordinate([], _).
stampa_canzoni_ordinate([PunteggioPonderato-Titolo | Rest], Posizione) :- 
    canzone(Titolo, Artista, Genere, _),
    write(Posizione), write('# '), write(Titolo), 
    write(' (Artista: '), write(Artista), write(', Genere: '), write(Genere), 
    format(' , Punteggio ponderato: ~2f\n', [PunteggioPonderato]),
    stampa_canzoni_ordinate(Rest, Posizione + 1).

/* Predicati ausiliari */
mostra_generi_preferiti :- 
    findall(Genere-Peso, genere_preferito(Genere, Peso), Generi),
    (   Generi == []
    ->  write('Non è stato definito alcun genere preferito.\n')
    ;   write('I tuoi generi preferiti e i loro pesi:\n'),
        stampa_generi(Generi)
    ).

mostra_generi_disponibili :- 
    findall(Genere, canzone(_, _, Genere, _), Generi),
    sort(Generi, GeneriUnici),
    write('Generi disponibili: '), write(GeneriUnici), nl.

stampa_generi([]).
stampa_generi([Genere-Peso | Rest]) :- 
    write(Genere), write(': '), write(Peso), nl,
    stampa_generi(Rest).

peso_genere(Genere, Peso) :- 
    (   genere_preferito(Genere, Peso) -> true ; Peso = 1 ).

ordina_punteggi([], []).
ordina_punteggi([H|T], Sorted) :- 
    ordina_punteggi(T, SortedT),
    inserisci_in_ordinato(H, SortedT, Sorted).

inserisci_in_ordinato(X, [], [X]).
inserisci_in_ordinato(X, [Y|Ys], [X,Y|Ys]) :- 
    number(X), number(Y), X >= Y, !.
inserisci_in_ordinato(X, [Y|Ys], [Y|Zs]) :- 
    number(X), number(Y), inserisci_in_ordinato(X, Ys, Zs).

/* ================================================ 
   Predicati principali 
   ================================================ */

main :- 
    write('Benvenuto nel sistema di raccomandazione musicale!\n'),
    write('Per utilizzare il sistema, segui i passaggi indicati.\n\n'),
    write('1. Carica le canzoni con: carica_canzoni.\n'),
    carica_canzoni,
    write('2. Puoi inserire i generi musicali che preferisci con: gestisci_generi_preferiti.\n'),
    gestisci_generi_preferiti,
    write('3. Stampa la classifica delle canzoni con: stampa_classifica.\n'),
    stampa_classifica,
    write('4. Stampa la lista dei generi preferiti con: mostra_generi_preferiti.\n'),
    mostra_generi_preferiti.
