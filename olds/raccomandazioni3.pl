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
    ->  write('Canzoni caricate con successo!\n'),
        mostra_generi_disponibili
    ;   write('\nErrore nel caricamento del file. Riprova.\n')
    ).

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
    read_line_to_string(Stream, Line),
    (   Line \= end_of_file
    ->  write('Linea letta: '), write(Line), nl,
        split_string(Line, ",", " ", [Titolo, Artista, Genere, PunteggioStr]),
        number_string(Punteggio, PunteggioStr),
        assertz(canzone(Titolo, Artista, Genere, Punteggio)),
        leggi_canzoni(Stream)
    ;   true ).

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

/* Predicato che calcola il punteggio ponderato per ogni canzone 
   in base al suo genere e al suo punteggio originale.
   Poi stampa la classifica ordinata delle canzoni. */
stampa_classifica :- 
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
    stampa_canzoni_ordinate(Rest, Posizione + 1).

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
   univoci presenti nel database delle canzoni. */
mostra_generi_disponibili :- 
    findall(Genere, canzone(_, _, Genere, _), Generi),
    sort(Generi, GeneriUnici),
    format('Generi disponibili: ~w\n', [GeneriUnici]).

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

/* Predicato che 'peso_genere' restituisce il peso di un genere.
   Se non è specificato, viene utilizzato un peso di 1. */
peso_genere(Genere, Peso) :- 
    (   genere_preferito(Genere, Peso) -> true ; Peso = 1 ).

/* Predicato che rimuove gli spazi da una stringa. */
rimuovi_spazi(Stringa, StringaRimossa) :- 
    string_codes(Stringa, Codici),
    rimuovi_spazi_codici(Codici, CodiciRimossi),
    string_codes(StringaRimossa, CodiciRimossi).

/* Predicato che rimuove gli spazi dalla lista
   di codici ASCII di una stringa. */
rimuovi_spazi_codici([], []).
rimuovi_spazi_codici([32|T], R) :- rimuovi_spazi_codici(T, R).
rimuovi_spazi_codici([H|T], [H|R]) :- H \= 32, rimuovi_spazi_codici(T, R).