% 2.1 Predicado extrai_ilhas_linha/3

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

% 2.2 Predicado ilhas/2

ilhas(Puz, Ilhas_Final) :- ilhas(Puz, Ilhas, 1),
    append(Ilhas, Ilhas_Final).

ilhas([],[],_).

ilhas([L | R], [Ilhas_Linha | Ilhas_Resto], I) :-
    extrai_ilhas_linha(I, L, Ilhas_Linha),
    NI is I+1,
    ilhas(R, Ilhas_Resto, NI).

% 2.3 Predicado vizinhas/3

vizinhas(Ilhas, ilha(_, (PY1, PX1)), Vizinhas) :-
    findall(
        IlhaVizinha, 
        (   
            member(IlhaVizinha, Ilhas), 
            IlhaVizinha = ilha(_, (PY2, PX2)), 
            (
                (PX1 == PX2, PY1>PY2)
            )
         
        ), 
        IlhaCima
    ),
    (last(IlhaCima, UltimaCima), Cima = [UltimaCima] ; Cima = []),
    

    findall(
        IlhaVizinha, 
        (   
            member(IlhaVizinha, Ilhas), 
            IlhaVizinha = ilha(_, (PY2, PX2)), 
            (
                (PX1 == PX2, PY1<PY2)
            )
         
        ), 
        IlhaBaixo
    ),
    (IlhaBaixo = [UltimaBaixo | _], Baixo = [UltimaBaixo] ; Baixo = []),

    findall(
        IlhaVizinha, 
        (   
            member(IlhaVizinha, Ilhas), 
            IlhaVizinha = ilha(_, (PY2, PX2)), 
            (
                (PY1 == PY2, PX1>PX2)
            )
         
        ), 
        IlhaEsquerda
    ),
    (last(IlhaEsquerda, UltimaEsquerda), Esquerda = [UltimaEsquerda] ; Esquerda = []),

    findall(
        IlhaVizinha, 
        (   
            member(IlhaVizinha, Ilhas), 
            IlhaVizinha = ilha(_, (PY2, PX2)), 
            (
                (PY1 == PY2, PX1<PX2)
            )
         
        ), 
        IlhaDireita
    ),
    (IlhaDireita = [UltimaDireita | _], Direita = [UltimaDireita] ; Direita = []),
    
    append([Cima, Esquerda, Direita, Baixo], Vizinhas).

% 2.4 Predicado estado/2

estado(Ilhas, Estado) :- estado(Ilhas, Estado, Ilhas).

estado([], [], _).

estado([Ilha | Resto], [[Ilha, Vizinhas, []] | RestoEstado], Ilhas) :-
    vizinhas(Ilhas, Ilha, Vizinhas),
    estado(Resto,RestoEstado,Ilhas).

% 2.5 Predicado posicoes_entre/3

posicoes_entre((X, PY1), (X, PY2), Posicoes) :-
    Y1 is PY1+1,
    Y2 is PY2-1,
    findall((X,Y), between(Y1, Y2, Y), Posicoes).

posicoes_entre((PX1, Y), (PX2, Y), Posicoes) :-
    X1 is PX1+1,
    X2 is PX2-1,
    findall((X,Y), between(X1, X2, X), Posicoes).


% 2.6 Predicado cria_ponte/3

cria_ponte((X1, Y1), (X2, Y2), ponte((X1, Y1), (X2, Y2))) :- (X1<X2 ; (X1==X2, Y1<Y2)), !.
cria_ponte((X1, Y1), (X2, Y2), ponte((X2, Y2), (X1, Y1))) :- (X1>X2 ; (X1==X2, Y1>Y2)).

% 2.7 Predicado caminho_livre/5

caminho_livre(Pos1, Pos2, _, ilha(_,Pos_I), ilha(_,Pos_Vz)) :-
    Pos1 == Pos_I,
    Pos2 == Pos_Vz,
    !.

caminho_livre(_, _, Posicoes, ilha(_,Pos_I), ilha(_,Pos_Vz)) :-
    posicoes_entre(Pos_I,Pos_Vz,Pos_Entre),
    \+ (member(Pos, Posicoes),
    member(Pos, Pos_Entre)).

% 2.8 Predicado actualiza_vizinhas_entrada/5

actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, [Ilha, Vizinhas, Pontes], [Ilha, NovasVizinhas, Pontes]) :-
    findall(Vizinha, (member(Vizinha,Vizinhas), caminho_livre(Pos1,Pos2,Posicoes, Ilha, Vizinha)), NovasVizinhas).

% 2.9 Predicado actualiza_vizinhas_apos_pontes/4

actualiza_vizinhas_apos_pontes([],_,_,[]).

actualiza_vizinhas_apos_pontes([Entrada | Resto_Estado], Pos1, Pos2, [NovaEntrada | Resto_Novo_Estado]) :-
    posicoes_entre(Pos1, Pos2, Posicoes),
    actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, Entrada, NovaEntrada),
    actualiza_vizinhas_apos_pontes(Resto_Estado, Pos1, Pos2, Resto_Novo_Estado).