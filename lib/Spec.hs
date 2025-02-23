module Main where

import Test.HUnit
import LI12324
import Tarefa1
import Tarefa2
import Tarefa3

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
j2 = Personagem {velocidade = (0.0, 0.0), tipo = Jogador, posicao = (5.0, 10.5), direcao = Oeste, tamanho = (1, 1), emEscada = True, ressalta = False, vida = 10, pontos = 0, aplicaDano = (False, 0)}
j3 = Personagem {velocidade = (0.0, 0.0), tipo = Jogador, posicao = (4.4, 10.5), direcao = Oeste, tamanho = (1, 1), emEscada = True, ressalta = False, vida = 10, pontos = 0, aplicaDano = (False, 0)}


i1 = Personagem {velocidade = (0.0, 0.0), tipo = Fantasma, posicao = (4.0, 10.5), direcao = Oeste, tamanho = (1, 1), emEscada = True, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False, 0)}
i2 = Personagem {velocidade = (0.0, 0.0), tipo = MacacoMalvado, posicao = (4.4, 10.5), direcao = Oeste, tamanho = (1, 1), emEscada = True, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False, 0)}
i = [i1,i2]

c = [(Moeda,(1.5,1.5))]

teste1 = TestLabel "T1" $ test [teste1A, teste1B, teste1C, teste1D]
  where
    teste1A = "A: Personagem colide com parede" ~: True ~=? colisoesParede m j1
    teste1B = "B: Personagem colide com parede" ~: False ~=? colisoesParede m j2
    teste1C = "C: Personagens colidem" ~: False ~=? colisoesPersonagens j1 i1
    teste1D = "D: Personagens colidem" ~: True ~=? colisoesPersonagens j2 i1

teste2 = TestLabel "T2" $ test [teste2A, teste2B]
    where
    teste2A = "A: O personagem obdece a todas as condições" ~: True ~=? valida Jogo{mapa = m, inimigos = i, jogador = j1, colecionaveis = c}
    teste2B = "B: O personagem obdece a todas as condições" ~: False ~=? valida Jogo {mapa = m, inimigos = i, jogador = j3, colecionaveis = c}




testes = TestLabel "Tarefa2 (valida)" $ test [teste1, teste2]

main :: IO ()
main = runTestTTAndExit $ test [teste1, teste2]
