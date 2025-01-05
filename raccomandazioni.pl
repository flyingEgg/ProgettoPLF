/* ######################################################### */
/* # Corso di Programmazione Logica e Funzionale           # */
/* # Progetto di raccomandazione di canzoni                # */
/* # Studente: Giaconi Christian, Giacomo Rossi            # */
/* # Matricola: 314045, 314671                             # */
/* ######################################################### */

:- dynamic canzone/4.
:- dynamic genere_preferito/2.

/* ================================================
   Predicati principali
   ================================================ */

main :- 
    write('Benvenuto nel sistema di raccomandazione musicale!\n'),
    loop_menu.

/* Ciclo che gestisce la selezione delle azioni */
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

/* Predicato per caricare le canzoni da un file */
carica_canzoni_interattivo :- 
    write('Inserisci il nome del file contenente le canzoni: '), nl,
    read(File),
    (   carica_canzoni(File)
    ->  write('Canzoni caricate con successo!\n'), nl,
        trova_generi_disponibili(Generi),
        format('Generi disponibili nel file: ~w\n', [Generi]), nl
    ;   write('Errore nel caricamento del file. Riprova.\n') ).

/* Predicato per gestire i generi preferiti */
gestisci_generi_preferiti :- 
    trova_generi_disponibili(Generi),
    format('Generi disponibili: ~w\n', [Generi]), nl,
    write('Inserisci i tuoi generi preferiti, uno per volta. Inserisci "fine" per terminare: '), nl,
    chiedi_generi_preferiti([]).

/* Predicato che chiede e gestisce l'inserimento dei generi preferiti */
chiedi_generi_preferiti(GeneriPreferiti) :- 
    write('Inserisci un genere preferito: '),
    read(Genere),
    normalizza_genere(Genere, GenereNormalizzato),
    (   Genere == fine
    ->  format('Generi preferiti separati: ~w\n', [GeneriPreferiti]),
        chiedi_peso_generi(GeneriPreferiti)
    ;   append(GeneriPreferiti, [GenereNormalizzato], NuoviGeneri),
        chiedi_generi_preferiti(NuoviGeneri) ).

/* Predicato che chiede il peso per ciascun genere preferito */
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

/* Predicato per caricare le canzoni da un file */
carica_canzoni(File) :- 
    open(File, read, Stream),
    leggi_canzoni(Stream),
    close(Stream).

/* Predicato che legge le canzoni dal file */
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

/* Predicato per trovare i generi disponibili nel file */
trova_generi_disponibili(Generi) :- 
    findall(Genere, canzone(_, _, Genere, _), GeneriDuplicati),
    sort(GeneriDuplicati, Generi).

/* Predicato per calcolare e stampare la classifica delle canzoni */
stampa_classifica :- 
    write('Classifica delle canzoni in base al punteggio ponderato:\n'),
    findall(PunteggioPonderato-Titolo, calcola_punteggio_ponderato(Titolo, PunteggioPonderato), Punteggi),
    (   Punteggi == []
    ->  write('Nessuna canzone trovata con punteggio ponderato.\n')
    ;   sort(1, @>=, Punteggi, Ordinata),
        stampa_canzoni_ordinate(Ordinata, 1)
    ).

/* Predicato che calcola il punteggio ponderato di una canzone */
calcola_punteggio_ponderato(Titolo, PunteggioPonderato) :- 
    canzone(Titolo, _, Genere, Punteggio),
    normalizza_genere(Genere, GenereNormalizzato),
    peso_genere(GenereNormalizzato, Peso),
    PunteggioPonderato is Punteggio * Peso.

/* Predicato che stampa le canzoni ordinate per punteggio */
stampa_canzoni_ordinate([], _).
stampa_canzoni_ordinate([PunteggioPonderato-Titolo | Rest], Posizione) :- 
    canzone(Titolo, Artista, Genere, _),
    format('~d# ~w (Artista: ~w, Genere: ~w, Punteggio ponderato: ~2f)\n', 
           [Posizione, Titolo, Artista, Genere, PunteggioPonderato]),
    NuovaPosizione is Posizione + 1,
    stampa_canzoni_ordinate(Rest, NuovaPosizione).

/* Predicato per normalizzare il genere */
normalizza_genere(Genere, GenereNormalizzato) :- 
    string_lower(Genere, GenereMinuscolo),
    rimuovi_spazi(GenereMinuscolo, GenereNormalizzato).

/* Predicato per ottenere il peso di un genere */
peso_genere(Genere, Peso) :- 
    (   genere_preferito(Genere, Peso)
    ->  true
    ;   Peso = 1 ).

/* Predicato per rimuovere gli spazi da una stringa */
rimuovi_spazi(Stringa, StringaRimossa) :- 
    string_codes(Stringa, Codici),
    rimuovi_spazi_codici(Codici, CodiciRimossi),
    string_codes(StringaRimossa, CodiciRimossi).

/* Predicato che rimuove gli spazi dalla lista di codici */
rimuovi_spazi_codici([], []).
rimuovi_spazi_codici([32|T], CodiciRimossi) :- 
    rimuovi_spazi_codici(T, CodiciRimossi).
rimuovi_spazi_codici([32], []).
rimuovi_spazi_codici([H|T], [H|CodiciRimossi]) :- 
    H \= 32,
    rimuovi_spazi_codici(T, CodiciRimossi).
