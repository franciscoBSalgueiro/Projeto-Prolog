% Francisco Salgueiro - num 103345

:-[codigo_comum].

% 2.1 Predicado extrai_ilhas_linha/3

% extrai_ilhas_linha(N_L, Linha, Ilhas), em que N_L e um inteiro positivo,
% correspondente ao numero de uma linha e Linha e uma lista correspondente a uma linha
% de um puzzle, significa que Ilhas e a lista ordenada (ilhas da esquerda para a direita)
% cujos elementos sao as ilhas da linha Linha.

extrai_ilhas_linha(N_L, Linha, Ilhas) :- extrai_ilhas_linha(N_L, Linha, Ilhas, 1).

extrai_ilhas_linha(_, [], [], _).

extrai_ilhas_linha(N_L, [Coluna | Linha], [ilha(Coluna,(N_L,N_C)) | Ilhas], N_C) :-
    Coluna\=0,
    !,
    N_C_1 is N_C+1,
    extrai_ilhas_linha(N_L, Linha, Ilhas, N_C_1).

extrai_ilhas_linha(N_L, [_ | Linha], Ilhas, N_C) :-
    N_C_1 is N_C+1,
    extrai_ilhas_linha(N_L, Linha, Ilhas, N_C_1).

% 2.2 Predicado ilhas/2

% ilhas(Puz, Ilhas), em que Puz e um puzzle, significa que Ilhas e a lista ordenada
% (ilhas da esquerda para a direita e de cima para baixo) cujos elementos sao as ilhas de
% Puz.

ilhas(Puz, Ilhas_Final) :- ilhas(Puz, Ilhas, 1),
    append(Ilhas, Ilhas_Final). % usa-se o append para eliminar as listas vazias

ilhas([],[],_).

ilhas([L | R], [Ilhas_Linha | Ilhas_Resto], I) :-
    extrai_ilhas_linha(I, L, Ilhas_Linha),
    I_1 is I+1,
    ilhas(R, Ilhas_Resto, I_1).

% 2.3 Predicado vizinhas/3

% vizinhas(Ilhas, Ilha, Vizinhas), em que Ilhas e a lista de ilhas de um puzzle
% e Ilha e uma dessas ilhas, significa que Vizinhas e a lista ordenada (ilhas de cima para
% baixo e da esquerda para a direita ) cujos elementos sao as ilhas vizinhas de Ilha.

cima(PX1, PY1, ilha(_, (PY2, PX1))) :- PY1>PY2.

baixo(PX1, PY1, ilha(_, (PY2, PX1))) :- PY1<PY2.

esquerda(PX1, PY1, ilha(_, (PY1, PX2))) :- PX1>PX2.

direita(PX1, PY1, ilha(_, (PY1, PX2))) :- PX1<PX2.

primeiro_em_lista([],[]).
primeiro_em_lista([P | _],[P]).

ultimo_em_lista([],[]).
ultimo_em_lista(Ilhas,[Ultima]) :-
    last(Ilhas, Ultima).

vizinhas(Ilhas, ilha(_, (PY1, PX1)), Vizinhas) :-
    include(cima(PX1, PY1),Ilhas,IlhaCima),
    ultimo_em_lista(IlhaCima,Cima),

    include(baixo(PX1, PY1),Ilhas,IlhaBaixo),
    primeiro_em_lista(IlhaBaixo,Baixo),

    include(esquerda(PX1, PY1),Ilhas,IlhaEsquerda),
    ultimo_em_lista(IlhaEsquerda,Esquerda),

    include(direita(PX1, PY1),Ilhas,IlhaDireita),
    primeiro_em_lista(IlhaDireita,Direita),

    % o append eh usado para eliminar listas vazias
    append([Cima, Esquerda, Direita, Baixo], Vizinhas).

% 2.4 Predicado estado/2

% estado(Ilhas, Estado), em que Ilhas e a lista de ilhas de um puzzle, significa que
% Estado e a lista ordenada cujos elementos sao as entradas referentes a cada uma das
% ilhas de Ilhas.

estado(Ilhas, Estado) :- estado(Ilhas, Estado, Ilhas).

estado([], [], _).

estado([Ilha | Resto], [[Ilha, Vizinhas, []] | RestoEstado], Ilhas) :-
    vizinhas(Ilhas, Ilha, Vizinhas),
    estado(Resto,RestoEstado,Ilhas).

% 2.5 Predicado posicoes_entre/3

% posicoes_entre(Pos1, Pos2, Posicoes), em que Pos1 e Pos2 sao posicoes, sig-
% nifica que Posicoes e a lista ordenada de posicoes entre Pos1 e Pos2 (excluindo Pos1 e
% Pos2). Se Pos1 e Pos2 nao pertencerem a mesma linha ou a mesma coluna, o resultado
% e false.

posicoes_entre((PX1, PY1), (PX2, PY2), Posicoes) :-
    (PX1=PX2 ; PY1=PY2),
    X1 is min(PX1,PX2),
    X2 is max(PX1,PX2),
    Y1 is min(PY1,PY2),
    Y2 is max(PY1,PY2),
    findall((X,Y), (between(X1, X2, X), between(Y1, Y2, Y), (X,Y)\=(PX1, PY1),(X,Y)\=(PX2, PY2)), Posicoes).

% 2.6 Predicado cria_ponte/3

% cria_ponte(Pos1, Pos2, Ponte), em que Pos1 e Pos2 sao 2 posicoes, significa
% que Ponte e uma ponte entre essas 2 posicoes.

cria_ponte((X1, Y1), (X2, Y2), ponte((X1, Y1), (X2, Y2))) :- (X1<X2 ; (X1==X2, Y1<Y2)), !.
cria_ponte(Pos1, Pos2, ponte(Pos2, Pos1)).

% 2.7 Predicado caminho_livre/5

% caminho_livre(Pos1, Pos2, Posicoes, I, Vz), em que Pos1 e Pos2 sao po-
% sicoes, Posicoes e a lista ordenada de posicoes entre Pos1 e Pos2, I e uma ilha, e Vz
% e uma das suas vizinhas, significa que a adicao da ponte ponte(Pos1, Pos2) nao faz
% com que I e Vz deixem de ser vizinhas.

caminho_livre(Pos1, Pos2, _, ilha(_,Pos1), ilha(_,Pos2)).
caminho_livre(Pos1, Pos2, _, ilha(_,Pos2), ilha(_,Pos1)).

caminho_livre(_, _, Posicoes, ilha(_,Pos_I), ilha(_,Pos_Vz)) :-
    posicoes_entre(Pos_I,Pos_Vz,Pos_Entre),
    \+ (member(Pos, Posicoes),
    member(Pos, Pos_Entre)).

% 2.8 Predicado actualiza_vizinhas_entrada/5

% actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, Entrada,
% Nova_Entrada), em que Pos1 e Pos2 sao as posicoes entre as quais ira ser adi-
% cionada uma ponte, Posicoes e a lista ordenada de posicoes entre Pos1 e Pos2,
% e Entrada e uma entrada , significa que Nova_Entrada e igual a
% Entrada, excepto no que diz respeito a lista de ilhas vizinhas; esta deve ser actualizada,
% removendo as ilhas que deixaram de ser vizinhas, apos a adicao da ponte.

actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, [Ilha, Vizinhas, Pontes], [Ilha, NovasVizinhas, Pontes]) :-
    include(caminho_livre(Pos1,Pos2,Posicoes,Ilha), Vizinhas, NovasVizinhas).

% 2.9 Predicado actualiza_vizinhas_apos_pontes/4

% actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, Novo_estado) ,
% em que Estado e um estado , Pos1 e Pos2 sao as posicoes entre as
% quais foi adicionada uma ponte, significa que Novo_estado e o estado que se obtem de
% Estado apos a actualizacao das ilhas vizinhas de cada uma das suas entradas.

actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, Novo_estado) :-
    posicoes_entre(Pos1, Pos2, Posicoes),
    maplist(actualiza_vizinhas_entrada(Pos1,Pos2,Posicoes), Estado, Novo_estado).

% 2.10 Predicado ilhas_terminadas/2

% ilhas_terminadas(Estado, Ilhas_term), em que Estado e um estado,
% significa que Ilhas_term e a lista de ilhas que ja tem todas as pontes associadas,
% designadas por ilhas terminadas. Se a entrada referente a uma ilha for [ilha(N_pontes,
% Pos), Vizinhas, Pontes], esta ilha esta terminada se N_pontes for diferente de
% 'X' (a razao para esta condicao ficara aparente mais a frente) e o comprimento da lista
% Pontes for N_pontes 

ilhas_terminadas(Estado, Ilhas_term) :-
    findall(Ilha, (member([Ilha,_,L], Estado), Ilha = ilha(V,_), integer(V), length(L, V)), Ilhas_term).

% 2.11 Predicado tira_ilhas_terminadas_entrada/3

% tira_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_entrada),
% em que Ilhas_term e uma lista de ilhas terminadas e Entrada e uma entrada (ver
% Seccao 2.4), significa que Nova_entrada e a entrada resultante de remover as ilhas de
% Ilhas_term, da lista de ilhas vizinhas de entrada.

tira_ilhas_terminadas_entrada(Ilhas_term, [Ilha, Vizinhas, Pontes], [Ilha, NovasVizinhas, Pontes]) :-
    findall(IlhaNaoTerminada, (member(IlhaNaoTerminada, Vizinhas), \+ member(IlhaNaoTerminada, Ilhas_term)), NovasVizinhas).

% 2.12 Predicado tira_ilhas_terminadas/3

% tira_ilhas_terminadas(Estado, Ilhas_term, Novo_estado), em que
% Estado e um estado e Ilhas_term e uma lista de ilhas termi-
% nadas, significa que Novo_estado e o estado resultante de aplicar o predicado
% tira_ilhas_terminadas_entrada a cada uma das entradas de Estado

tira_ilhas_terminadas(Estado, Ilhas_term, Novo_estado) :-
    maplist(tira_ilhas_terminadas_entrada(Ilhas_term), Estado, Novo_estado).

% 2.13 Predicado marca_ilhas_terminadas_entrada/3

% marca_ilhas_terminadas_entrada(Ilhas_term, Entrada,
% Nova_entrada), em que Ilhas_term e uma lista de ilhas terminadas e Entrada
% e uma entrada , significa que Nova_entrada e a entrada obtida de
% Entrada da seguinte forma: se a ilha de Entrada pertencer a Ilhas_term, o numero
% de pontes desta e substituido por 'X'; em caso contrario Nova_entrada e igual a
% Entrada

marca_ilhas_terminadas_entrada(Ilhas_term, [ilha(V, Pos), Vizinhas, Pontes], [ilha('X', Pos), Vizinhas, Pontes]) :-
    member(ilha(V, Pos), Ilhas_term), !.

marca_ilhas_terminadas_entrada(_,Entrada,Entrada).

% 2.14 Predicado marca_ilhas_terminadas/3

% marca_ilhas_terminadas(Estado, Ilhas_term, Novo_estado), em que
% Estado e um estado e Ilhas_term e uma lista de ilhas termi-
% nadas, significa que Novo_estado e o estado resultante de aplicar o predicado
% marca_ilhas_terminadas_entrada a cada uma das entradas de Estado

marca_ilhas_terminadas(Estado, Ilhas_term, Novo_estado) :-
    maplist(marca_ilhas_terminadas_entrada(Ilhas_term), Estado, Novo_estado).

% 2.15 Predicado trata_ilhas_terminadas/2

% trata_ilhas_terminadas(Estado, Novo_estado), em que Estado e um estado,
% significa que Novo_estado e o estado resultante de aplicar os predica-
% dos tira_ilhas_terminadas e marca_ilhas_terminadas a Estado

trata_ilhas_terminadas(Estado, Novo_estado) :- 
    ilhas_terminadas(Estado,Ilhas_term),
    tira_ilhas_terminadas(Estado,Ilhas_term,Estado2),
    marca_ilhas_terminadas(Estado2,Ilhas_term,Novo_estado).

% 2.16 Predicado junta_pontes/5

% junta_pontes(Estado, Num_pontes, Ilha1, Ilha2, Novo_estado), em
% que Estado e um estado e Ilha1 e Ilha2 sao 2 ilhas, significa que Novo_estado e
% o estado que se obtem de Estado por adicao de Num_pontes pontes entre Ilha1 e
% Ilha2 

adiciona_ponte(Num_pontes, Ilha1, Ilha2, [Ilha, Vizinhas, Pontes], [Ilha, Vizinhas, NovasPontes]) :-
    (Ilha=Ilha1 ; Ilha=Ilha2) ->
        Ilha1 = ilha(_,Pos1), Ilha2= ilha(_,Pos2),
        cria_ponte(Pos1,Pos2,Ponte),
        findall(Ponte, between(1, Num_pontes, _), NovasPontes)
    ; NovasPontes = Pontes.

junta_pontes(Estado, Num_pontes, Ilha1, Ilha2, Novo_estado) :-
    maplist(adiciona_ponte(Num_pontes,Ilha1,Ilha2), Estado, Estado2),
    Ilha1 = ilha(_, Pos1),
    Ilha2 = ilha(_, Pos2),
    actualiza_vizinhas_apos_pontes(Estado2,Pos1,Pos2,Estado3),
    trata_ilhas_terminadas(Estado3,Novo_estado).
