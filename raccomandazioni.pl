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
   alle canzoni caricate. Ogni canzone è rappresentata 
   dai seguenti argomenti: Titolo, Artista, Genere e Punteggio. */
:- dynamic(canzone/4).

/* Predicato che 'genere_preferito/2' associa un peso preferito
   a ciascun genere musicale. Il primo argomento è il Genere,
   il secondo è il Peso associato a quel genere. */
:- dynamic(genere_preferito/2).

/* ================================================
   Predicati principali
   ================================================ */

/* Predicato che 'main' è il punto di ingresso principale.
   Inizializza il programma e avvia il menu interattivo
   per l'utente. */
main :- 
    write('Benvenuto nel sistema di raccomandazione musicale!'),
    carica_canzoni,
    loop_menu.

/* Predicato che 'loop_menu' gestisce la selezione delle azioni
   da parte dell'utente nel menu principale. Ogni opzione del menu
   chiama un predicato specifico per eseguire l'azione corrispondente. */
loop_menu :- 
    write('\nScegli un\'azione:\n'),
    write('1. Gestisci i generi preferiti (aggiungi o modifica)\n'),
    write('2. Stampa la classifica delle canzoni\n'),
    write('3. Stampa la lista dei generi preferiti\n'),
    write('4. Esci\n'),
    read(Scelta),
    (   Scelta = 1 -> gestisci_generi_preferiti
    ;   Scelta = 2 -> stampa_classifica
    ;   Scelta = 3 -> mostra_generi_preferiti
    ;   Scelta = 4 -> write('Arrivederci!\n'), halt
    ;   write('Scelta non valida. Riprova.\n')
    ),
    loop_menu.

carica_canzoni :- 
    assertz(canzone('Despacito', 'Luis Fonsi', 'Reggaeton', 8)),
    assertz(canzone('All Eyez On Me', 'Tupac', 'HipHop', 8)),
    assertz(canzone('Danza Kuduro', 'Don Omar', 'Reggaeton', 9)),
    assertz(canzone('Song 2', 'Blur', 'Alternative/Indie', 6)),
    assertz(canzone('Bachata Rosa', 'Juan Luis Guerra', 'Bachata', 9)),
    assertz(canzone('Notturno op 55 no 1', 'Chopin', 'Classica', 6)),
    assertz(canzone('Free Bird', 'Lynyrd Skynyrd', 'Rock', 8)),
    assertz(canzone('Thunderstruck', 'AC/DC', 'Rock', 7)),
    assertz(canzone('Come As You Are', 'Nirvana', 'Rock', 8)),
    assertz(canzone('La Gota Fria', 'Carlos Vives', 'Vallenato', 7)),
    assertz(canzone('Stronger', 'Kanye West', 'HipHop', 9)),
    assertz(canzone('Californication', 'Red Hot Chili Peppers', 'Alternative/Indie', 6)),
    assertz(canzone('Upper Echelon', 'Travis Scott', 'Trap', 7)),
    assertz(canzone('El Cantante', 'Hector Lavoe', 'Salsa', 9)),
    assertz(canzone('Suavemente', 'Elvis Crespo', 'Merengue', 10)),
    assertz(canzone('La Vaca', 'Los Toros Band', 'Merengue', 9)).

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
    ;   mostra_generi_disponibili,  % Stampa i generi disponibili
        findall(GenereDisponibile, canzone(_, _, GenereDisponibile, _), GeneriDisponibili), % Ottieni i generi disponibili
        (   membro(Genere, GeneriDisponibili)
        ->  append(GeneriPreferiti, [Genere], NuoviGeneri),
            chiedi_generi_preferiti(NuoviGeneri)
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
    Posizione1 is Posizione + 1,
    stampa_canzoni_ordinate(Rest, Posizione1).

/* Predicato che calcola il punteggio ponderato per ogni canzone 
   in base al suo genere e al suo punteggio originale.
   Poi stampa la classifica ordinata delle canzoni. */
stampa_classifica :- 
    findall(PunteggioPonderato-Titolo, calcola_punteggio_ponderato(Titolo, PunteggioPonderato), Punteggi),
    (   Punteggi == []
    ->  write('Nessuna canzone trovata con punteggio ponderato.\n')
    ;   ordina_lista(Punteggi, PunteggiOrdinati),
        stampa_canzoni_ordinate(PunteggiOrdinati, 1)
    ).

/* Predicato che calcola il punteggio ponderato
   di una canzone in base al suo genere (e al peso preferito associato)
   moltiplicato per il punteggio originale della canzone. */
calcola_punteggio_ponderato(Titolo, PunteggioPonderato) :- 
    canzone(Titolo, _, Genere, Punteggio),
    peso_genere(Genere, Peso),
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

/* Predicato che restituisce una lista dei generi musicali 
   presenti nel database delle canzoni, evitando duplicati. */
mostra_generi_disponibili :- 
    findall(Genere, canzone(_, _, Genere, _), Generi),
    elimina_duplicati(Generi, GeneriUnici),
    write('Generi disponibili:\n'),
    scrivi_lista(GeneriUnici).

/* Predicato che elimina duplicati da una lista. */
elimina_duplicati([], []).
elimina_duplicati([H|T], [H|T1]) :- 
    non_membro(H, T),
    elimina_duplicati(T, T1).
elimina_duplicati([H|T], T1) :- 
    membro(H, T),
    elimina_duplicati(T, T1).

/* Predicato che verifica se un elemento è membro della lista. */
membro(X, [X|_]).
membro(X, [_|T]) :- 
    membro(X, T).

/* Predicato che verifica se un elemento NON è membro della lista. */
non_membro(_, []).
non_membro(X, [H|T]) :- 
    X \= H,
    non_membro(X, T).

/* Scrive una lista elemento per elemento */
scrivi_lista([]).
scrivi_lista([H|T]) :- 
    write('- '), write(H), nl,
    scrivi_lista(T).

/* Predicato che stampa la lista dei generi preferiti. */
stampa_generi([]).
stampa_generi([Genere-Peso | Rest]) :- 
    format('~w: ~w\n', [Genere, Peso]),
    stampa_generi(Rest).

/* Predicato che 'peso_genere' restituisce il peso di un genere.
   Se non è specificato, viene utilizzato un peso di 1. */
peso_genere(Genere, Peso) :- 
    (   genere_preferito(Genere, Peso) -> true ; Peso = 1 ).

/* Predicato che ordina una lista in ordine decrescente. */
ordina_lista(Lista, Ordinata) :-
    ordina_lista(Lista, [], Ordinata).

/* Predicato che ordina la lista ricorsivamente. */
ordina_lista([], Acc, Acc).
ordina_lista([X|Xs], Acc, Ordinata) :-
    inserisci_decrescente(X, Acc, NuovoAcc),
    ordina_lista(Xs, NuovoAcc, Ordinata).

/* Predicato che inserisce un elemento in una lista mantenendo
   l'ordine decrescente. */
inserisci_decrescente(X, [], [X]).
inserisci_decrescente(Punteggio-Titolo, [Y|Ys], [Punteggio-Titolo, Y|Ys]) :-
    Punteggio >= Y.
inserisci_decrescente(Punteggio-Titolo, [Punteggio1-Titolo1|Ys], [Punteggio1-Titolo1|NuovaLista]) :-
    Punteggio < Punteggio1,
    inserisci_decrescente(Punteggio-Titolo, Ys, NuovaLista).
