% Definizione delle canzoni e dei generi musicali
canzone("Shape of You", "pop").
canzone("Bohemian Rhapsody", "rock").
canzone("Blinding Lights", "pop").
canzone("Stairway to Heaven", "rock").
canzone("Levitating", "pop").
canzone("Nothing Else Matters", "rock").
canzone("Save Your Tears", "pop").

% Punteggio di gradimento delle canzoni per ogni utente
gradimento("Alice", "Shape of You", 8).
gradimento("Alice", "Bohemian Rhapsody", 6).
gradimento("Alice", "Blinding Lights", 9).
gradimento("Alice", "Stairway to Heaven", 7).
gradimento("Bob", "Shape of You", 7).
gradimento("Bob", "Blinding Lights", 6).
gradimento("Bob", "Levitating", 8).
gradimento("Bob", "Nothing Else Matters", 5).

% Preferenze musicali degli utenti
preferenza_genere("Alice", "pop").
preferenza_genere("Bob", "rock").

% Regola per consigliare canzoni in base alle preferenze e al punteggio di gradimento
raccomanda_canzoni(Utente, Canzone) :-
    preferenza_genere(Utente, Genere),
    canzone(Canzone, Genere),
    gradimento(Utente, Canzone, Punteggio),
    Punteggio >= 7.

% Visualizzare le raccomandazioni per un utente
mostra_raccomandazioni(Utente) :-
    findall(Canzone, raccomanda_canzoni(Utente, Canzone), Canzoni),
    write("Raccomandazioni per "), write(Utente), write(": "), nl,
    mostra_canzoni(Canzoni).

% Funzione per stampare la lista delle canzoni
mostra_canzoni([]).
mostra_canzoni([Canzone|AltreCanzoni]) :-
    write(Canzone), nl,
    mostra_canzoni(AltreCanzoni).
