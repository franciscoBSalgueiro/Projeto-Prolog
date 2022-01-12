extrai_ilhas_linha(N_L, Linha, Ilhas) :-
    findall(ilha(Coluna,Pos), (Pos = (N_L,N_C), member(Coluna, Linha), Coluna\=0, nth1(N_C, Linha, Coluna)), Ilhas).
