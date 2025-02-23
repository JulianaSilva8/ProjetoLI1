{-|
Module      : Tarefa2
Description : Valida jogo
Copyright   : Dinis José Tecedeiro da Costa <a106872@alunos.uminho.pt>
              Juliana Sofia Vaz da Silva <a105572@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 de LI1 em 2023/24.
-}
module Tarefa2 where

import LI12324
import Tarefa1

{-|Dados para utilizar nos exemplos de utilização:
m :: Mapa
m = Mapa ((8.5, 6.5), Este) (5, 1.5)
    [[Vazio,      Vazio,      Vazio,      Vazio,      Vazio,      Vazio,      Vazio,      Vazio,      Vazio,      Vazio],
     [Vazio,      Vazio,      Vazio,      Vazio,      Vazio,      Vazio,      Vazio,      Vazio,      Vazio,      Vazio],
     [Vazio,      Vazio,      Vazio,      Plataforma, Plataforma, Plataforma, Plataforma, Vazio,      Vazio,      Vazio],
     [Vazio,      Vazio,      Vazio,      Escada,     Vazio,      Vazio,      Escada,     Vazio,      Vazio,      Vazio],
     [Vazio,      Vazio,      Vazio,      Escada,     Vazio,      Vazio,      Escada,     Vazio,      Vazio,      Vazio],
     [Vazio,      Vazio,      Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Vazio,      Vazio],
     [Vazio,      Vazio,      Escada,     Vazio,      Vazio,      Vazio,      Vazio,      Escada,     Vazio,      Vazio],
     [Vazio,      Vazio,      Escada,     Vazio,      Vazio,      Vazio,      Vazio,      Escada,     Vazio,      Vazio],
     [Vazio,      Plataforma, Plataforma, Plataforma, Alcapao,    Alcapao,    Plataforma, Plataforma, Plataforma, Vazio],
     [Vazio,      Escada,     Vazio,      Vazio,      Vazio,      Vazio,      Plataforma, Vazio,      Escada,     Vazio],
     [Vazio,      Escada,     Vazio,      Vazio,      Vazio,      Vazio,      Plataforma, Vazio,      Escada,     Vazio],
     [Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]
    ]

j1 = Personagem {velocidade = (0.0, 0.0), tipo = Jogador, posicao = (5.5, 10.5), direcao = Oeste, tamanho = (1, 1), emEscada = True, ressalta = False, vida = 10, pontos = 0, aplicaDano = (False, 0)}
j2 = Personagem {velocidade = (0.0, 0.0), tipo = Jogador, posicao = (4.4, 10.5), direcao = Oeste, tamanho = (1, 1), emEscada = True, ressalta = False, vida = 10, pontos = 0, aplicaDano = (False, 0)}
j3 = Personagem {velocidade = (0.0, 0.0), tipo = Jogador, posicao = (7.5, 7.5), direcao = Oeste, tamanho = (1, 1), emEscada = True, ressalta = False, vida = 10, pontos = 0, aplicaDano = (False, 0)}

i1 = Personagem {velocidade = (0.0, 0.0), tipo = Fantasma, posicao = (4.0, 10.5), direcao = Oeste, tamanho = (1, 1), emEscada = True, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False, 0)}
i2 = Personagem {velocidade = (0.0, 0.0), tipo = MacacoMalvado, posicao = (4.4, 10.5), direcao = Oeste, tamanho = (1, 1), emEscada = True, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False, 0)}

i = [i1,i2]
c = [(Moeda,(1.5,1.5))]
-}

{-| Nesta segunda tarefa, o objetivo era verificar se o jogo obdece a diversas condições.
    Para facilitar, foi feito e testado cada condição à vez para que tudo estivesse a funcionar devidamente.
    Se a função devolver True quer dizer que é tudo válido.
    Se devolver False, é porque há alguma condição que não está a ser respeitada.

    === Exemplos de utilização:
    >>> valida Jogo{mapa = m, inimigos = i, jogador = j1, colecionaveis = c}
    True

    >>> valida Jogo {mapa = m, inimigos = i, jogador = j2, colecionaveis = c}
    False
-}

valida :: Jogo -> Bool
valida Jogo {mapa = (Mapa _ _ blocos) ,inimigos = ini, colecionaveis = col, jogador = jog@(Personagem {posicao = (x,y), tamanho = (larg,alt)})}
    = chao && res && pos jog ini && nIni && fant && escadas && alc && colec
    where

--1
        chao = all (== Plataforma) (last blocos) -- ^ A ultima linha do mapa, tem de ser toda do tipo Plataforma
--2
        res  = all ressalta ini && not (ressalta jog) -- ^Os inimigos ressaltam e o jogador não
--3
        pos :: Personagem -> [Personagem] -> Bool -- ^ A posição inicial do jogador não pode colidir com a dos inimigos
        pos jog [] = True
        pos jog (ini1:t) = not (colisoesPersonagens jog ini1) && pos jog t

--4
        nIni = length ini >= 2 -- ^ Os inimigos têm de ser 2 ou mais
--5
        fant = all (\f -> tipo f == Fantasma && vida f == 1 ) (filter (\f -> tipo f == Fantasma) ini) -- ^ Os fantasmas apenas têm uma vida
--6
        escadas = (((blocos !! floor y !! floor x) == Escada && ((blocos !! (floor y - 1) !! floor x) /= Alcapao    && (blocos !! (floor y + 1) !! floor x) /= Alcapao))   -- ^ bloco acima ou abaixo não é Alçapão
               && (((blocos !! floor y !! floor x) == Escada && ((blocos !! (floor y - 1) !! floor x) == Plataforma || (blocos !! (floor y + 1) !! floor x) == Plataforma))   -- ^ bloco acima ou abaixo é Plataforma
               ||  ((blocos !! floor y !! floor x) == Escada && ((blocos !! (floor y - 1) !! floor x) == Escada     || (blocos !! (floor y + 1) !! floor x) == Escada)))) -- ^ bloco acima ou abaixo é Escada
               ||   (blocos !! floor y !! floor x) == Vazio -- ^ para anular o caso de ele só poder estar em escadas
--7
        alc  = fst (tamanho jog) <= largAlc -- ^ os alçapões têm de ser mais largos que o jogador
                where
                largAlc = 1
--8
        colec = all (==True) (map f (col)) -- ^ o bloco de cada colecionável tem de ser Vazio
        f (colecionavel1,(xc,yc))= ((blocos !! floor yc ) !! floor xc) == Vazio
