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
   Dati di esempio: Le canzoni sono definite come fatti
   ================================================ */

/* canzone(Titolo, Artista, Genere, Punteggio). */
canzone('Despacito', 'Luis Fonsi', 'Reggaeton', 9).
canzone('Shape of You', 'Ed Sheeran', 'Pop', 8).
canzone('Blinding Lights', 'The Weeknd', 'Pop', 10).
canzone('Taki Taki', 'DJ Snake', 'Reggaeton', 7).
canzone('Billie Jean', 'Michael Jackson', 'Pop', 10).
canzone('A Dios Le Pido','Juanes','Rock Latino',8).

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
    peso_genere(Genere, Peso),   /* Trova il peso del genere */
    PunteggioPonderato is Punteggio * Peso.    /* Calcola il punteggio ponderato */

/* Gestione del peso per generi definiti e predefiniti */
peso_genere(Genere, Peso) :-
    genere_preferito(Genere, Peso), !.

peso_genere(_, 1).  /* Peso predefinito */

/* Dichiarazione dinamica del predicato genere_preferito */
:- dynamic genere_preferito/2.

/* Definisce i pesi per i generi musicali preferiti. */
genere_preferito('Rock Latino', 1.7).   /* Peso maggiore per il Reggaeton */
genere_preferito('Pop', 1.2).         /* Peso maggiore per il Pop */

/* ================================================
   Predicati per la gestione e ordinamento delle canzoni
   ================================================
   1. Calcola e ordina tutte le canzoni in base al punteggio ponderato.
   2. Stampa la classifica in ordine decrescente di punteggio.
*/

/* Ottiene la classifica delle canzoni ordinata per punteggio ponderato. */
classifica_ordinata(Ordinata) :-
    findall(Punteggio-Titolo, punteggio_ponderato(Titolo, Punteggio), Punteggi),
    sort(1, @>=, Punteggi, Ordinata).  /* Ordina in ordine decrescente */

/* Funzione per stampare la classifica */
stampa_classifica :- 
    classifica_ordinata(Ordinata),
    stampa_canzoni(Ordinata, 1).  /* Passa la posizione iniziale 1 */

/* Funzione per stampare la lista delle canzoni con i punteggi ponderati e la posizione */
stampa_canzoni([], _).  /* Caso base: se la lista è vuota, non stampare nulla */
stampa_canzoni([Punteggio-Titolo | Rest], Posizione) :-
    format('~d# Canzone: ~w, Punteggio ponderato: ~2f~n', [Posizione, Titolo, Punteggio]), /* Stampa posizione, titolo e punteggio */
    NuovaPosizione is Posizione + 1,  /* Incrementa la posizione */
    stampa_canzoni(Rest, NuovaPosizione).  /* Ricorsione per stampare le canzoni rimanenti */

/* Funzione per stampare la lista completa delle canzoni disponibili */
stampa_canzoni_presenti :-
    write('Lista completa di canzoni disponibili:'), nl,
    findall((Titolo, Artista, Genere), canzone(Titolo, Artista, Genere, _), Canzoni),
    forall(member((Titolo, Artista, Genere), Canzoni),
           format('Titolo: ~w, Artista: ~w, Genere: ~w~n', [Titolo, Artista, Genere])).

/* Funzione per stampare i generi preferiti e i loro pesi */
stampa_generi_preferiti :-
    write('Generi musicali preferiti e i loro pesi:'), nl,
    findall((Genere, Peso), genere_preferito(Genere, Peso), Generi),
    forall(member((Genere, Peso), Generi),
           format('Genere: ~w, Peso: ~2f~n', [Genere, Peso])).

/* ================================================
   Funzione per aggiungere o modificare un genere preferito
   ================================================
   Questo predicato consente di aggiungere nuovi generi preferiti o modificare
   quelli esistenti nel database.
*/

aggiungi_genere(Genere, Peso) :-
    Peso > 0,   /* Valida che il peso sia positivo */
    retractall(genere_preferito(Genere, _)),  /* Rimuove eventuali definizioni precedenti */
    assertz(genere_preferito(Genere, Peso)).  /* Aggiungi o modifica il peso del genere */

/* Funzione per resettare un genere preferito */
resetta_genere(Genere) :-
    retractall(genere_preferito(Genere, _)).  /* Rimuove tutte le definizioni per quel genere */

/* ================================================
   Predicati per aggiungere e rimuovere canzoni
   ============================================= */

/* Funzione per aggiungere una canzone alla classifica */
aggiungi_canzone(Titolo, Artista, Genere, Punteggio) :-
    Punteggio >= 1, Punteggio =< 10,  /* Valida che il punteggio sia tra 1 e 10 */
    assertz(canzone(Titolo, Artista, Genere, Punteggio)),
    (   \+ genere_preferito(Genere, _)  /* Se il genere non è presente */
    ->  aggiungi_genere(Genere, 1)     /* Aggiungi il genere con peso predefinito */
    ;   true).

/* Funzione per rimuovere una canzone dalla classifica */
rimuovi_canzone(Titolo) :-
    retractall(canzone(Titolo, _, _, _)).  /* Rimuove tutte le canzoni con il titolo specificato */

/* ================================================
   Funzione principale per avviare il programma
   ================================================
   Questo predicato mostra le istruzioni all'utente e stampa la classifica iniziale.
*/

main :-
    nl,
    write('Benvenuto nel sistema di raccomandazione musicale!'), nl,
    write('============================================'), nl,
    write('Comandi disponibili:'), nl,
    write('1. Visualizza la classifica delle canzoni: visualizza_classifica.'), nl,
    write('2. Aggiungi o modifica un genere preferito: aggiungi_genere(Genere, Peso).'), nl,
    write('3. Resetta un genere preferito: resetta_genere(Genere).'), nl,
    write('4. Aggiungi una nuova canzone: aggiungi_canzone(Titolo, Artista, Genere, Punteggio).'), nl,
    write('5. Rimuovi una canzone: rimuovi_canzone(Titolo).'), nl,
    write('============================================'), nl,
    write('Per utilizzare i comandi si devono inserire gli argomenti tra apici.'), nl,
    write('============================================'), nl,
    nl,
    write('Ecco la classifica iniziale dei suggerimenti delle canzoni:'), nl,
    stampa_classifica,
    nl,
    write('============================================'), nl,
    stampa_canzoni_presenti,
    nl,
    write('============================================'), nl,
    stampa_generi_preferiti.
