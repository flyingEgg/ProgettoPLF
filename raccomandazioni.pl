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

/* Funzione per leggere le canzoni da un file di testo */
carica_canzoni(File) :-
    open(File, read, Stream)
    leggi_canzoni(Stream),
    close(Stream)

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
   Dati di esempio: Le canzoni sono definite come fatti
   ================================================ */

/* canzone(Titolo, Artista, Genere, Punteggio). */
canzone('Despacito', 'Luis Fonsi', 'Reggaeton', 9).
canzone('Shape of You', 'Ed Sheeran', 'Pop', 8).
canzone('Blinding Lights', 'The Weeknd', 'Pop', 10).
canzone('Taki Taki', 'DJ Snake', 'Reggaeton', 7).
canzone('Billie Jean', 'Michael Jackson', 'Pop', 10).

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
    genere_preferito(Genere, Peso),   /* Trova il peso del genere */
    PunteggioPonderato is Punteggio * Peso.    /* Calcola il punteggio ponderato */

/* 
2. I generi preferiti e i loro pesi sono definiti nei fatti successivi.
   Se il genere è preferito, viene dato un peso maggiore, altrimenti il peso è 1.
*/

/* Definisce i pesi per i generi musicali preferiti. */
genere_preferito('Reggaeton', 1.5).   /* Peso maggiore per il Reggaeton */
genere_preferito('Pop', 1.2).         /* Peso maggiore per il Pop */
genere_preferito(_, 1).              /* Default: peso 1 per gli altri generi */

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

/* Funzione ausiliaria per stampare la lista delle canzoni ordinate. */
stampa_classifica :-
    classifica_ordinata(Ordinata),
    stampa_lista(Ordinata).

/* Stampa la lista delle canzoni con i punteggi ponderati */
stampa_lista([]).  /* Caso base: se la lista è vuota, non stampare nulla */
stampa_lista([Punteggio-Titolo | Rest]) :-
    format('Canzone: ~w, Punteggio ponderato: ~2f~n', [Titolo, Punteggio]), /* Stampa titolo e punteggio */
    stampa_lista(Rest).  /* Ricorsione per stampare le canzoni rimanenti */

/* ================================================
   Funzione per aggiungere o modificare un genere preferito
   ================================================
   Questo predicato consente di aggiungere nuovi generi preferiti o modificare
   quelli esistenti nel database.
*/

aggiungi_genere(Genere, Peso) :-
    retractall(genere_preferito(Genere, _)),  /* Rimuove eventuali definizioni precedenti */
    assertz(genere_preferito(Genere, Peso)).  /* Aggiungi o modifica il peso del genere */

/* Funzione per resettare un genere preferito */
resetta_genere(Genere) :-
    retractall(genere_preferito(Genere, _)).  /* Rimuove tutte le definizioni per quel genere */

/* ================================================
   Predicati per aggiungere e rimuovere canzoni
   ================================================ */

/* Funzione per aggiungere una canzone alla classifica */
aggiungi_canzone(Titolo, Artista, Genere, Punteggio) :-
    assertz(canzone(Titolo, Artista, Genere, Punteggio)).

/* Funzione per rimuovere una canzone dalla classifica */
rimuovi_canzone(Titolo) :-
    retractall(canzone(Titolo, _, _, _)).  /* Rimuove tutte le canzoni con il titolo specificato */

/* ================================================
   Funzione principale per avviare il programma
   ================================================
   Questo predicato mostra le istruzioni all'utente e gestisce l'interazione.
*/

main :-
    nl,
    write('Benvenuto nel sistema di raccomandazione musicale!'), nl,
    write('============================================'), nl,
    write('Istruzioni:'), nl,
    write('1. Per visualizzare la classifica delle canzoni, scrivi: visualizza_classifica.'), nl,
    write('2. Per aggiungere o modificare un genere preferito, scrivi: aggiungi_genere(Genere, Peso).'), nl,
    write('3. Per resettare un genere preferito, scrivi: resetta_genere(Genere).'), nl,
    write('4. Per aggiungere una canzone alla classifica, scrivi: aggiungi_canzone(Titolo, Artista, Genere, Punteggio).'), nl,
    write('5. Per rimuovere una canzone dalla classifica, scrivi: rimuovi_canzone(Titolo).'), nl,
    write('6. Per uscire, scrivi: halt.'), nl,
    write('============================================'), nl,
    write('Ora puoi iniziare a interagire con il programma.'), nl.

% ================================================
% Esempio di interazione:
% 1. Per visualizzare la classifica delle canzoni:
% ?- visualizza_classifica.

% 2. Per aggiungere un nuovo genere preferito (ad esempio "Jazz" con peso 1.8):
% ?- aggiungi_genere('Jazz', 1.8).

% 3. Per resettare il genere "Pop":
% ?- resetta_genere('Pop').

% 4. Per aggiungere una nuova canzone alla classifica:
% ?- aggiungi_canzone('New Song', 'Artist Name', 'Pop', 9).

% 5. Per rimuovere una canzone dalla classifica:
% ?- rimuovi_canzone('Shape of You').

% 6. Per vedere di nuovo la classifica:
% ?- visualizza_classifica.