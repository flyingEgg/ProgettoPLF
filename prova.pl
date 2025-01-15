
    :- dynamic(canzone/4).

    :- dynamic(stringa/1).

    :- dynamic(genere_preferito/2).


   main :-
       write('Programma avviato\n'),
       carica_stringhe,
       stringa(Stringz),
       riproduci_righe(Stringz).

   carica_stringhe :-
        assertz(stringa('Despacito,Luis Fonsi,Reggaeton,8')),
        assertz(stringa('All Eyez On Me,Tupac,HipHop,9')).

   riproduci_righe(Riga) :-
        split_string(Riga, ",", "", [Titolo, Artista, Genere, PunteggioStr]),
        number_charlist(Punteggio, PunteggioStr),
        assertz(canzone(Titolo, Artista, Genere, Punteggio)),
        format('~w (Artista: ~w, Genere: ~w, Punteggio ponderato: ~2f)\n',
                   [Titolo, Artista, Genere, Punteggio]).


   % split_string(+Input, +Delimiters, +Padding, -Result)
   split_string(Input, Delimiters, Padding, Result) :-
       atom_codes(Input, InputCodes),
       atom_codes(Delimiters, DelimCodes),
       atom_codes(Padding, PaddingCodes),
       remove_padding(InputCodes, PaddingCodes, TrimmedInput),
       split_codes(TrimmedInput, DelimCodes, [], Substrings),
       maplist(atom_codes, Result, Substrings).

   % remove_padding(+Codes, +PaddingCodes, -Trimmed)
   remove_padding([], _, []).
   remove_padding([H | T], PaddingCodes, Trimmed) :-
       member(H, PaddingCodes), !,
       remove_padding(T, PaddingCodes, Trimmed).
   remove_padding([H | T], PaddingCodes, [H | Trimmed]) :-
       remove_padding(T, PaddingCodes, Trimmed).

   % split_codes(+Codes, +DelimCodes, +Current, -Substrings)
   split_codes([], _, Current, [Current]) :- Current \= [].
   split_codes([], _, [], []).
   split_codes([H | T], DelimCodes, Current, [Current | Rest]) :-
       member(H, DelimCodes), !,
       split_codes(T, DelimCodes, [], Rest).
   split_codes([H | T], DelimCodes, Current, Result) :-
       \+ member(H, DelimCodes),
       append(Current, [H], NewCurrent),
       split_codes(T, DelimCodes, NewCurrent, Result).


   % number_charlist(+Number, -CharList)
   number_charlist(Number, CharList) :-
       number(Number), !,
       number_codes(Number, CharCodes), % Convert number to list of character codes
       maplist(code_to_char, CharCodes, CharList). % Convert codes to characters

   % number_charlist(-Number, +CharList)
   number_charlist(Number, CharList) :-
       is_list(CharList), % Ensure CharList is a list of characters
       maplist(char_id, CharList, CharCodes), % Convert characters to codes
       number_codes(Number, CharCodes). % Convert codes to number

   % char_code(+Char, -Code) or (+Code, -Char)
   char_to_code(Char, Code) :-
       char_code(Char, Code). % Convert character to its ASCII code


