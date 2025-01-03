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
   Sistema di raccomandazione di canzoni in Prolog
   ================================================
   Questo programma consente di raccomandare canzoni in base a:
   1. Generi musicali preferiti
   2. Punteggi ponderati per dare priorità alle canzoni più rilevanti
   L'utente può modificare i pesi dei generi e visualizzare le canzoni raccomandate
   L'utente può aggiungere e rimuovere canzoni dalla classifica.
   ================================================ */

/* ================================================
   Caricamento dinamico delle canzoni da file
   ================================================ */

/* Lettura delle canzoni da un file di testo */
carica_canzoni(File) :-
    open(File, read, Stream),
    leggi_canzoni(Stream),
    close(Stream).

leggi_canzoni(Stream) :-
    read_line_to_string(Stream, Riga),
    (   Riga \= end_of_file
    ->  split_string(Riga, ",", " ", [Titolo, Artista, Genere, PunteggioStr]),
        number_string(Punteggio, PunteggioStr),
        assertz(canzone(Titolo, Artista, Genere, Punteggio)),
        leggi_canzoni(Stream)
    ;   true
    ).

/* ================================================
   Regole e predicati per il calcolo dei punteggi ponderati
   ================================================
   1. Il punteggio ponderato di una canzone dipende dal suo genere e dal punteggio
      assegnato. Se il genere della canzone è tra quelli preferiti, il punteggio viene
      moltiplicato per un fattore di ponderazione maggiore.
*/

/* Calcola il punteggio ponderato di una canzone, basato sul genere preferito. */
punteggio_ponderato(Titolo, PunteggioPonderato) :-
    canzone(Titolo, _, Genere, Punteggio),
    peso_genere(Genere, Peso),                  /* Trova il peso del genere */
    PunteggioPonderato is Punteggio * Peso.     /* Calcola il punteggio ponderato */

/* Gestione del peso per generi definiti e predefiniti */
peso_genere(Genere, Peso) :-
    genere_preferito(Genere, Peso), !.

peso_genere(_, 1).  /* Peso predefinito */

/* Dichiarazione dinamica del predicato genere_preferito */
:- dynamic genere_preferito/2.

/* Definisce i pesi per i generi musicali preferiti. */
genere_preferito('Bachata', 1.7).       /* Peso maggiore per la Bachata */
genere_preferito('Merengue', 1.2).      /* Peso maggiore per il Merengue */

/* ================================================
   Predicati per la gestione e ordinamento delle canzoni
   ================================================
   1. Calcola e ordina tutte le canzoni in base al punteggio ponderato.
   2. Stampa la classifica in ordine decrescente di punteggio.
*/

/* Stampa la classifica ordinata */
stampa_classifica :-
    findall(Punteggio-Titolo, punteggio_ponderato(Titolo, Punteggio), Punteggi),
    sort(1, @>=, Punteggi, Ordinata),
    stampa_canzoni_ordinate(Ordinata, 1).

/* Stampa le canzoni ordinate in base al punteggio ponderato */
stampa_canzoni_ordinate([], _).     /* Caso base: se la lista è vuota, termina la funzione */
stampa_canzoni_ordinate([Punteggio-Titolo | Rest], Posizione) :- 
    canzone(Titolo, Artista, Genere, _),
    format('~d# Canzone: ~w~n   Artista: ~w~n   Genere: ~w~n   Punteggio ponderato: ~2f~n~n',
           [Posizione, Titolo, Artista, Genere, Punteggio]),
    NuovaPosizione is Posizione + 1,
    stampa_canzoni_ordinate(Rest, NuovaPosizione).

/* Stampa le canzoni ordinate come da file */
stampa_canzoni_file([], _).     /* Caso base: se la lista è vuota, termina la funzione */
stampa_canzoni_file([Punteggio-Titolo | Rest], Posizione) :-
    canzone(Titolo, Artista, Genere, _),
    format('~d# Canzone: ~w~n   Artista: ~w~n   Genere: ~w~n   Punteggio ponderato: ~2f~n~n',
           [Posizione, Titolo, Artista, Genere, Punteggio]),
    NuovaPosizione is Posizione + 1,
    stampa_canzoni(Rest, NuovaPosizione).

/* Stampa i generi preferiti */
stampa_generi_preferiti :-
    write('Generi musicali preferiti e i loro pesi:'), nl,
    findall((Genere, Peso), genere_preferito(Genere, Peso), Generi),
    forall(member((Genere, Peso), Generi),
           format('Genere: ~w, Peso: ~2f~n', [Genere, Peso])).

/* Stampa delle le canzoni disponibili */
stampa_canzoni_disponibili :-
    write('Lista completa di canzoni disponibili:'), nl,
    findall((Titolo, Artista, Genere), canzone(Titolo, Artista, Genere, _), Canzoni),
    forall(member((Titolo, Artista, Genere), Canzoni),
           format('Titolo: ~w, Artista: ~w, Genere: ~w~n', [Titolo, Artista, Genere])).

/* ================================================
   Aggiunta o rimozione di un genere preferito
   ================================================
   Questo predicato consente di aggiungere nuovi generi preferiti o modificare
   quelli esistenti nel database.
*/

aggiungi_genere_preferito(Genere, Peso) :-
    Peso > 0,   /* Valida che il peso sia positivo */
    retractall(genere_preferito(Genere, _)),  /* Rimuove eventuali definizioni precedenti */
    assertz(genere_preferito(Genere, Peso)).  /* Aggiungi o modifica il peso del genere */

/* Funzione per rimuovere un genere preferito */
rimuovi_genere_preferito(Genere) :-
    retractall(genere_preferito(Genere, _)).  /* Rimuove tutte le definizioni per quel genere */

/* ================================================
   Funzione principale per avviare il programma
   ================================================
   Questo predicato mostra le istruzioni all'utente e stampa la classifica iniziale.
*/

main :-
    nl,
    write('Benvenuto nel sistema di raccomandazione musicale!'), nl,
    write('============================================'), nl,
    write('Per iniziare, carica un file di canzoni con il comando seguente:'), nl,
    write('  carica_canzoni(nomefile.txt).'), nl,
    write('IMPORTANTE: PER UTILIZZARE I COMANDI SI DEVONO INSERIRE GLI ARGOMENTI TRA APICI'), nl,
    write('============================================'), nl,
    write('Comandi disponibili:'), nl,
    write('1. Visualizza la classifica delle canzoni: stampa_classifica.'), nl,
    write('2. Visualizza le canzoni caricate: stampa_canzoni_presenti.'), nl,
    write('3. Visualizza i generi preferiti: stampa_generi_preferiti.'), nl,
    write('4. Aggiungi o modifica un genere preferito: aggiungi_genere_preferito(Genere, Peso).'), nl,
    write('5. Rimuovi un genere preferito: rimuovi_genere_preferito(Genere).'), nl,
    write('============================================'), nl,
    stampa_generi_preferiti,
    write('============================================'), nl.
