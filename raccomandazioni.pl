/* #########################################################
# Corso di Programmazione Logica e Funzionale              #
# Progetto di raccomandazione di canzoni                   #
# Studente: [Il tuo Nome]                                  #
# Matricola: [Il tuo Numero]                               #
######################################################### */

/* Specifica:
   Scrivere un programma Prolog per implementare un sistema di raccomandazione di canzoni.
   Il sistema suggerisce canzoni basandosi sulle preferenze dell'utente (es. genere musicale)
   e utilizza un punteggio di gradimento per ordinare le canzoni più popolari o rilevanti.
*/

/* Fatti: rappresentano le canzoni disponibili nel sistema.
   canzone(Titolo, Artista, Genere, Punteggio).
*/
canzone("Blinding Lights", "The Weeknd", "Pop", 9).
canzone("Smells Like Teen Spirit", "Nirvana", "Rock", 8).
canzone("Shape of You", "Ed Sheeran", "Pop", 7).
canzone("Imagine", "John Lennon", "Classic", 10).
canzone("Bohemian Rhapsody", "Queen", "Rock", 9).

/* Predicato principale per avviare il programma. */
main :-
    write("Benvenuto al sistema di raccomandazione di canzoni!"), nl,
    write("Inserire il genere musicale preferito (es. Pop): "), nl,
    read(GenerePreferito),
    raccomanda(GenerePreferito, ListaCanzoni),
    write("Ecco le canzoni raccomandate per te:"), nl,
    stampaLista(ListaCanzoni).

/* Predicato per raccomandare canzoni in base al genere preferito. */
raccomanda(Genere, ListaOrdinata) :-
    findall([Titolo, Artista, Punteggio],
            canzone(Titolo, Artista, Genere, Punteggio),
            ListaNonOrdinata),
    sort(3, @>=, ListaNonOrdinata, ListaOrdinata).

/* Predicato per stampare una lista di canzoni in modo leggibile. */
stampaLista([]).
stampaLista([[Titolo, Artista, Punteggio] | Coda]) :-
    format("~w - ~w, Punteggio: ~w~n", [Titolo, Artista, Punteggio]),
    stampaLista(Coda).
