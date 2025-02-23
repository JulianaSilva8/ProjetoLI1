{-|
Module      : Tarefa4
Description : Atualiza as velocidades das personagens no jogo
Copyright   : Dinis José Tecedeiro da Costa <a106872@alunos.uminho.pt>
              Juliana Sofia Vaz da Silva <a105572@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 de LI1 em 2023/24.
-}
module Tarefa4 where

import Data.Maybe

import LI12324
import Tarefa1

{-| o objetivo desta tarefa é validar e calcular as direcoes e velocidades de todos os personagens, de acordo com as açoes que eles recebem
  Os dados a utilizar nos exemplos sao :
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

j1 = Personagem {velocidade = (0.0, 0.0), tipo = Jogador, posicao = (5.5, 10.5), direcao = Oeste, tamanho = (1, 1), emEscada = True, ressalta = False, vida = 10, pontos = 0, aplicaDano = (True, 10)}

i1 = Personagem {velocidade = (0.0, 0.0), tipo = Fantasma, posicao = (5.0, 10.5), direcao = Oeste, tamanho = (1, 1), emEscada = True, ressalta = False, vida = 2, pontos = 0, aplicaDano = (False, 0)}
i2 = Personagem {velocidade = (0.0, 0.0), tipo = MacacoMalvado, posicao = (4.4, 10.5), direcao = Oeste, tamanho = (1, 1), emEscada = True, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False, 0)}
i = [i1,i2]
c = [(Moeda,(1.5,1.5))]


acaoJog = Just Subir
acaoIni = [Nothing, Just Saltar] -}

{-| A função atualiza recebe uma lista de Maybe acao, um Maybe acao e um Jogo e devolve as consequências do que aconteceu no jogo.
    Usa as funções jogAtualizado, iniAtualizado para atualizar cada personagem.

    == Exemplos:
    >>> atualiza acaoIni acaoJog Jogo {mapa = m, inimigos = i, jogador = j1, colecionaveis = col}
    Jogo {mapa = Mapa ((8.5,6.5),Este) (5.0,1.5) [[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
                                                  [Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
                                                  [Vazio,Vazio,Vazio,Plataforma,Plataforma,Plataforma,Plataforma,Vazio,Vazio,Vazio],
                                                  [Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Escada,Vazio,Vazio,Vazio],
                                                  [Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Escada,Vazio,Vazio,Vazio],
                                                  [Vazio,Vazio,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Vazio,Vazio],
                                                  [Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio],
                                                  [Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio],
                                                  [Vazio,Plataforma,Plataforma,Plataforma,Alcapao,Alcapao,Plataforma,Plataforma,Plataforma,Vazio],
                                                  [Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Plataforma,Vazio,Escada,Vazio],
                                                  [Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Plataforma,Vazio,Escada,Vazio],
                                                  [Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma]],
            inimigos = [Personagem {velocidade = (0.0,0.0), tipo = Fantasma, posicao = (5.0,10.5), direcao = Oeste, tamanho = (1.0,1.0), emEscada = True, ressalta = False, vida = 1, pontos = 0, aplicaDano = (False,0.0)},
                        Personagem {velocidade = (0.0,0.0), tipo = MacacoMalvado, posicao = (-1.0,-1.0), direcao = Oeste, tamanho = (1.0,1.0), emEscada = True, ressalta = True, vida = 0, pontos = 0, aplicaDano = (False,0.0)}],
            colecionaveis = [(Moeda,(1.5,1.5))],
            jogador = Personagem {velocidade = (0.0,0.0), tipo = Jogador, posicao = (5.5,10.5), direcao = Oeste, tamanho = (1.0,1.0), emEscada = True, ressalta = False, vida = 10, pontos = 0, aplicaDano = (True,9.0)}}
-}


atualiza :: [Maybe Acao] -> Maybe Acao -> Jogo -> Jogo
atualiza acaoIni acaoJog jogo@(Jogo {mapa = (Mapa (pi,di) pf blocos) ,inimigos = ini, jogador = jog@(Personagem {posicao = (x,y), tamanho = (larg,alt)}) , colecionaveis = col})
    = Jogo {mapa = Mapa (pi,di) pf blocos, jogador = jogAtualizado, inimigos = iniAtualizados, colecionaveis = col}
  where
     
    jogAtualizado = atualizaJog acaoJog jog
    iniAtualizados = zipWith atualizaIni acaoIni ini

    atualizaJog maybeAcao jog =
        case maybeAcao of
        Just acao -> atualizaPersonagem acao jog
        Nothing   -> jog
    
    atualizaIni maybeAcao ini =  
        case maybeAcao of
          Just acao -> atualizaPersonagem acao ini
          Nothing   -> ini

    atualizaPersonagem acao jog =
        case acao of
        Saltar -> if not platCima
                  then jog { velocidade = (0.0,-1.0), posicao = (fst (posicao jog), snd (posicao jog) + snd (velocidade jog)) }
                  else jog
                      where
                      intervaloY = [y + x / 10 | y <- [0..fromIntegral (length (head blocos))], x <- [1, 2, 3, 4, 5]] -- ^ intervalo com valores valores y.1 .2 .3 .4 .5 para que y.5 estivesse sempre incluso
                      platCima = if elem y intervaloY && ((x<x2) && (x2<x+larg)) -- ^ está no intervalo e o x vai até .4 do bloco à direita e .6 à esquerda
                                 then blocos !! floor (y2 - 1) !! floor x1 == Plataforma
                                   || blocos !! floor (y2 - 1) !! floor x1 == Alcapao
                                 else blocos !! floor y2 !! floor x1 == Plataforma
                                   || blocos !! floor y2 !! floor x1 == Alcapao

        Subir  -> if naEscada || noBloco
                  then jog {velocidade = (0.0,-0.2), posicao = (fst (posicao jog), snd (posicao jog) + snd ( velocidade jog)), emEscada = True}
                  else jog
                    where
                    naEscada = blocos !! ceiling y2 !! floor x == Escada
                    noBloco = blocos !! ceiling y2 !! floor x == Plataforma


        Descer -> if naEscada
                  then jog {velocidade =  (0.0,0.2), posicao = (fst (posicao jog), snd (posicao jog) + snd (velocidade jog)), emEscada = True}
                  else jog
                      where
                      naEscada = blocos !! ceiling y2 !! floor x == Escada
                      --escChao = blocos !! ceiling y2 !! floor x1 == P


        AndarDireita  -> if not platDir
                         then jog {velocidade = (0.2,0.0), direcao = Este, posicao = (fst (posicao jog) + fst (velocidade jog), snd (posicao jog))}
                         else jog
                            where
                            platDir  = blocos !! (floor y1 -1) !! floor x2 == Plataforma
                                    || blocos !! (floor y1 -1) !! floor x2 == Alcapao

        AndarEsquerda -> if not platEsq
                         then jog {velocidade = (-0.2,0.0), direcao = Oeste, posicao = (fst (posicao jog) + fst (velocidade jog), snd (posicao jog))}
                         else jog
                            where -- retirada da Tarefa1
                            intervaloX = [x + y / 10 | x <- [0..fromIntegral (length blocos)], y <- [1, 2, 3, 4, 5]] -- ^ intervalo com valores valores x.1 .2 .3 .4 .5 para que x.5 estivesse sempre incluso
                            platEsq = if elem x intervaloX && ((y<y1) && (y1<y+alt)) -- ^ está no intervalo e o y vai até .4 do bloco abaixo e .6 a cima
                                      then blocos !! (floor y1 -1) !! (ceiling x1 - 1) == Plataforma
                                        || blocos !! (floor y1 -1) !! (ceiling x1 - 1) == Alcapao
                                      else blocos !! (floor y1 -1) !! floor x1 == Plataforma
                                        || blocos !! (floor y1 -1) !! floor x1 == Alcapao

        Parar  -> jog {velocidade = (0.0,0.0)}



    hb = ((x1,y1),(x2,y2))
    x1 = x - larg/2
    x2 = x + larg/2
    y1 = y + alt/2
    y2 = y - alt/2