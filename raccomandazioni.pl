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
:- dynamic canzone/4.

/* Predicato che 'genere_preferito/2' associa un peso preferito
   a ciascun genere musicale. Il primo argomento è il Genere,
   il secondo è il Peso associato a quel genere. */
:- dynamic genere_preferito/2.

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
    write('Scegli un\'azione:\n'),
    write('1. Carica un file con le canzoni\n'),
    write('2. Gestisci i generi preferiti (aggiungi o modifica)\n'),
    write('3. Stampa la classifica delle canzoni\n'),
    write('4. Esci\n'),
    read(Scelta),
    (   Scelta = 1 -> carica_canzoni_interattivo,
        loop_menu
    ;   Scelta = 2 -> gestisci_generi_preferiti,
        loop_menu
    ;   Scelta = 3 -> stampa_classifica,
        loop_menu
    ;   Scelta = 4 -> write('Arrivederci!\n'), halt
    ;   write('Scelta non valida. Riprova.\n'),
        loop_menu).

/* ================================================
   Predicati di caricamento delle canzoni
   ================================================ */

/* Predicato che permette all'utente di inserire il nome del
   file che contiene le canzoni.
   Se il caricamento ha successo, stampa un messaggio di conferma. */
carica_canzoni_interattivo :- 
    write('Inserisci il nome del file contenente le canzoni: '), nl,
    read(File),
    (   carica_canzoni(File)
    ->  write('Canzoni caricate con successo!\n'), nl,
        trova_generi_disponibili(Generi),
        format('Generi disponibili nel file: ~w\n', [Generi]), nl
    ;   write('Errore nel caricamento del file. Riprova.\n') ).

/* Predicato che apre il file specificato e legge le canzoni
   riga per riga, aggiungendole al database. */
carica_canzoni(File) :- 
    open(File, read, Stream),
    leggi_canzoni(Stream),
    close(Stream).

/* Predicato che legge il file e per ogni riga estrae il
   titolo, l'artista, il genere e il punteggio della canzone.
   Le informazioni vengono memorizzate nel database. */
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

/* ================================================
   Predicati per la gestione dei generi preferiti
   ================================================ */

/* Predicato che permette all'utente di selezionare 
   e gestire i generi musicali preferiti. */
gestisci_generi_preferiti :- 
    trova_generi_disponibili(Generi),
    format('Generi disponibili: ~w\n', [Generi]), nl,
    write('Inserisci i tuoi generi preferiti, uno per volta. Inserisci "fine" per terminare: '), nl,
    chiedi_generi_preferiti([]).

/* Predicato che raccoglie i generi preferiti inseriti
   dall'utente e li aggiunge alla lista di preferiti. */
chiedi_generi_preferiti(GeneriPreferiti) :- 
    write('Inserisci un genere preferito: '),
    read(Genere),
    normalizza_genere(Genere, GenereNormalizzato),
    (   Genere == fine
    ->  format('Generi preferiti separati: ~w\n', [GeneriPreferiti]),
        chiedi_peso_generi(GeneriPreferiti)
    ;   append(GeneriPreferiti, [GenereNormalizzato], NuoviGeneri),
        chiedi_generi_preferiti(NuoviGeneri) ).

/* Predicato che chiede all'utente di inserire
   un peso per ciascun genere musicale preferito. */
chiedi_peso_generi([]).
chiedi_peso_generi([Genere | Altri]) :- 
    format('Inserisci il peso per il genere ~w: ', [Genere]),
    read(Peso),
    (   number(Peso), Peso > 0
    ->  (   retractall(genere_preferito(Genere, _)),
            assertz(genere_preferito(Genere, Peso))
        ),
        chiedi_peso_generi(Altri)
    ;   write('Peso non valido. Riprova.\n'),
        chiedi_peso_generi([Genere | Altri]) ).

/* Predicato che restituisce una lista dei generi musicali 
   univoci presenti nel database delle canzoni. */
trova_generi_disponibili(Generi) :- 
    findall(Genere, canzone(_, _, Genere, _), GeneriDuplicati),
    sort(GeneriDuplicati, Generi).

/* ================================================
   Predicati per la raccomandazione e la classifica
   ================================================ */

/* Predicato che calcola il punteggio ponderato per ogni canzone 
   in base al suo genere e al suo punteggio originale.
   Poi stampa la classifica ordinata delle canzoni. */
stampa_classifica :- 
    write('Classifica delle canzoni in base al punteggio ponderato:\n'),
    findall(PunteggioPonderato-Titolo, calcola_punteggio_ponderato(Titolo, PunteggioPonderato), Punteggi),
    (   Punteggi == []
    ->  write('Nessuna canzone trovata con punteggio ponderato.\n')
    ;   sort(1, @>=, Punteggi, Ordinata),
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

/* Predicato che stampa le canzoni ordinate in base al punteggio
   ponderato, elencandole con la posizione, il titolo,
   l'artista e il punteggio ponderato. */
stampa_canzoni_ordinate([], _).
stampa_canzoni_ordinate([PunteggioPonderato-Titolo | Rest], Posizione) :- 
    canzone(Titolo, Artista, Genere, _),
    format('~d# ~w (Artista: ~w, Genere: ~w, Punteggio ponderato: ~2f)\n', 
           [Posizione, Titolo, Artista, Genere, PunteggioPonderato]),
    NuovaPosizione is Posizione + 1,
    stampa_canzoni_ordinate(Rest, NuovaPosizione).

/* ================================================
   Predicati ausiliari
   ================================================ */

/* Predicato che normalizza il genere, 
   trasformandolo in minuscolo e rimuovendo eventuali spazi. */
normalizza_genere(Genere, GenereNormalizzato) :- 
    string_lower(Genere, GenereMinuscolo),
    rimuovi_spazi(GenereMinuscolo, GenereNormalizzato).

/* Predicato che 'peso_genere' restituisce il peso di un genere.
   Se non è specificato, viene utilizzato un peso di 1. */
peso_genere(Genere, Peso) :- 
    (   genere_preferito(Genere, Peso)
    ->  true
    ;   Peso = 1 ).

/* Predicato che rimuove gli spazi da una stringa. */
rimuovi_spazi(Stringa, StringaRimossa) :- 
    string_codes(Stringa, Codici),
    rimuovi_spazi_codici(Codici, CodiciRimossi),
    string_codes(StringaRimossa, CodiciRimossi).

/* Predicato che rimuove gli spazi dalla lista
   di codici ASCII di una stringa. */
rimuovi_spazi_codici([], []).
rimuovi_spazi_codici([32|T], CodiciRimossi) :- 
    rimuovi_spazi_codici(T, CodiciRimossi).
rimuovi_spazi_codici([32], []).
rimuovi_spazi_codici([H|T], [H|CodiciRimossi]) :- 
    H \= 32,
    rimuovi_spazi_codici(T, CodiciRimossi).
