{-|
Module      : Tarefa1
Description : Verifica colisões
Copyright   : Dinis José Tecedeiro da Costa <a106872@alunos.uminho.pt>
              Juliana Sofia Vaz da Silva <a105572@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 de LI1 em 2023/24.
-}
module Tarefa1 where

import LI12324

{- | O  primeiro objetivo desta tarefa era verificar se o jogador colide com as laterais do mapa ou com um bloco do tipo Plataforma (pela lateral ou topo).
     O segundo objetivo era verificar se dois personagens colidem entre si.
-}

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
j2 = Personagem {velocidade = (0.0, 0.0), tipo = Jogador, posicao = (5.0, 10.5), direcao = Oeste, tamanho = (1, 1), emEscada = True, ressalta = False, vida = 10, pontos = 0, aplicaDano = (False, 0)}

j3 = Personagem {velocidade = (0.0, 0.0), tipo = Jogador, posicao = (0.5, 0.5), direcao = Oeste, tamanho = (1, 1), emEscada = True, ressalta = False, vida = 10, pontos = 0, aplicaDano = (False, 0)}

i1 = Personagem {velocidade = (0.0, 0.0), tipo = Jogador, posicao = (4.0, 10.5), direcao = Oeste, tamanho = (1, 1), emEscada = True, ressalta = False, vida = 10, pontos = 0, aplicaDano = (False, 0)}
-}

{- | Primeiramente, foi verificado se um personagem colide com as laterais do mapa (tanto no eixo do x como do y).
     De seguida, verificou-se se o personagem colide com algum bloco de plataforma (em cima, à esquerda e à direita). Para isso, foi feita a hitbox do personagem para que a colisão
    entre ele e os blocos de plataforma fosse mais fácil. Aqui ocorreram várias dificuldades devido aos arredondamentos mas após fazer inumeras tentativas, percebemos que havia um padrão.
     Assim, usamos intervalos para conseguir fazer os arredondamentos para que tudo funcionasse bem.
     Depois disso, foi só juntar todas as função numa só chamada colisoesParede que verifica se há ou não uma colisão.
     Se colidir : True
     Se não colidir : False

    === Exemplo de utilização:
    >>> colisoesParede m j1
    True

    >>> colisoesParede m j2
    False

    >>> colisoesParede m j3
    True
-}


colisoesParede :: Mapa -> Personagem -> Bool
colisoesParede (Mapa _ _ blocos) (Personagem {posicao = (x,y) , tamanho = (larg,alt)})
  = laterais || plataformaDir  || plataformaEsq  || plataformaCima
      where
      laterais = x1 <= 0 || y2 <= 0
              || y1 >= fromIntegral (length blocos)
              || x2 >= fromIntegral (length (head blocos))


      hb = ((x1,y1),(x2,y2))
      x1 = x - larg/2
      x2 = x + larg/2
      y1 = y + alt/2
      y2 = y - alt/2


      plataformaDir  = blocos !! (floor y1 -1) !! floor x2 == Plataforma
                    || blocos !! (floor y1 -1) !! floor x2 == Alcapao



      intervaloX = [x + y / 10 | x <- [0..fromIntegral (length blocos)], y <- [1, 2, 3, 4, 5]] -- ^ intervalo com valores valores x.1 .2 .3 .4 .5 para que x.5 estivesse sempre incluso
      plataformaEsq  = if elem x intervaloX && ((y<y1) && (y1<y+alt)) -- ^ está no intervalo e o y vai até .4 do bloco abaixo e .6 a cima
                       then blocos !! (floor y1 -1) !! (ceiling x1 - 1) == Plataforma
                         || blocos !! (floor y1 -1) !! (ceiling x1 - 1) == Alcapao
                       else blocos !! (floor y1 -1) !! floor x1 == Plataforma
                         || blocos !! (floor y1 -1) !! floor x1 == Alcapao

      intervaloY = [y + x / 10 | y <- [0..fromIntegral (length (head blocos))], x <- [1, 2, 3, 4, 5]] -- ^ intervalo com valores valores y.1 .2 .3 .4 .5 para que y.5 estivesse sempre incluso
      plataformaCima = if elem y intervaloY && ((x<x2) && (x2<x+larg)) -- ^ está no intervalo e o x vai até .4 do bloco à direita e .6 à esquerda
                       then blocos !! floor (y2 - 1) !! floor x1 == Plataforma
                         || blocos !! floor (y2 - 1) !! floor x1 == Alcapao
                       else blocos !! floor y2 !! floor x1 == Plataforma
                         || blocos !! floor y2 !! floor x1 == Alcapao



{- | Esta função verifica se um personagem colide com outro.
     Aqui percebemos que seria mais facil se o foco fosse apenas um personagem e assim, verificavasse se havia colisão em qualquer um dos seus lados.
    Aqui não foi usada a hitbox diretamente, apenas se fez a soma ou subtração da altura/largura do personagem dependendo do lado que quisesse verificar a colisão.
    Assim, se qualquer um dos lados colide, então a função colisoesPersonagens dá True. Portanto:
    Se colidir : True
    Se não colidir : False

    === Exemplos de utilização:
    >>> colisoesPersonagens j1 i1
    False

    >>> colisoesPersonagens j2 i1
    True
-}

colisoesPersonagens :: Personagem -> Personagem -> Bool
colisoesPersonagens (Personagem {posicao = (x1,y1) , tamanho = (larg1,alt1)}) (Personagem {posicao = (x2,y2) , tamanho = (larg2,alt2)})
      =  (colisaoDir && colisaoEsq) && (colisaoCima && colisaoBaix)
      where
      colisaoDir  = x1 + (larg1/2) >= x2 - (larg2/2) -- ^ Calcula a colisão à direita de um personagem com outro
      colisaoEsq  = x1 - (larg1/2) <= x2 + (larg2/2) -- ^ Calcula a colisão à esquerda de um personagem com outro
      colisaoCima = y1 - (alt1 /2) <= y2 + (alt2 /2) -- ^ Calcula a colisão em cima de um personagem com outro
      colisaoBaix = y1 + (alt1 /2) >= y2 - (alt2 /2) -- ^ Calcula a colisão em baixo de um personagem com outro