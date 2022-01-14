extrai_ilhas_linha(N_L, Linha, Ilhas) :- extrai_ilhas_linha(N_L, Linha, Ilhas, 1).

extrai_ilhas_linha(_, [], [], _).

extrai_ilhas_linha(N_L, [Coluna | Linha], Ilhas, N_C) :-
    Coluna\=0,
    !,
    N_C2 is N_C+1,
    extrai_ilhas_linha(N_L, Linha, Ilhas2, N_C2),
    append([ilha(Coluna,(N_L,N_C))], Ilhas2, Ilhas).

extrai_ilhas_linha(N_L, [Coluna | Linha], Ilhas, N_C) :-
    Coluna==0,
    N_C2 is N_C+1,
    extrai_ilhas_linha(N_L, Linha, Ilhas, N_C2).

posicoes_entre(Pos1, Pos2, Posicoes) :-
    (PX1, PY1) = Pos1, 
    (PX2, PY2) = Pos2,
    (PX1==PX2 ; PY1==PY2),
    findall((X,Y), 
    (between(PX1, PX2, X), between(PY1, PY2, Y)),
    Posicoes).

% :- posicoes_entre((2,6), (1,6), Posicoes), writeln(Posicoes); writeln(false).
% output: [(1,3),(1,4),(1,5)]
:- posicoes_entre((2,6), (8,6), Posicoes), writeln(Posicoes); writeln(false).
% output: [(3,6),(4,6),(5,6),(6,6),(7,6)]
