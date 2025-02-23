{-|
Module      : Tarefa3
Description : Movimenta personagens no jogo
Copyright   : Dinis José Tecedeiro da Costa <a106872@alunos.uminho.pt>
              Juliana Sofia Vaz da Silva <a105572@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 de LI1 em 2023/24.
-}
module Tarefa3 where

import LI12324
import Data.List
import Tarefa1

{-| O objetivo desta tarefa é "atualizar" todos os campos de Jogo, e devolver a atualização do mesmo após mudanças de posições ou condições.
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

j1 = Personagem {velocidade = (0.0, 0.0), tipo = Jogador, posicao = (5.5, 10.5), direcao = Oeste, tamanho = (1, 1), emEscada = True, ressalta = False, vida = 10, pontos = 0, aplicaDano = (True, 10)}

i1 = Personagem {velocidade = (0.0, 0.0), tipo = Fantasma, posicao = (5.0, 10.5), direcao = Oeste, tamanho = (1, 1), emEscada = True, ressalta = False, vida = 2, pontos = 0, aplicaDano = (False, 0)}
i2 = Personagem {velocidade = (0.0, 0.0), tipo = MacacoMalvado, posicao = (4.4, 10.5), direcao = Oeste, tamanho = (1, 1), emEscada = True, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False, 0)}
i = [i1,i2]
c = [(Moeda,(1.5,1.5))]
-}

{-| A função movimenta recebe uma semente, um tempo e um Jogo e devolve as consequências do que aconteceu no jogo.
    Usa funções como atualizaMapa, atualizaJog, atualizaIni e atualizaCol para separar a atualização de todo o Jogo.

    === Exemplos de utilização:
    >>> movimenta 1 1.0 Jogo {mapa = m, inimigos = i, jogador = j1, colecionaveis = c}
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

movimenta :: Semente -> Tempo -> Jogo -> Jogo
movimenta sem temp jogo@(Jogo {mapa = (Mapa (pi,di) pe blocos) ,inimigos = ini, jogador = jog , colecionaveis = col})
     = Jogo {mapa = atualizaMapa, jogador = atualizaJog, inimigos = atualizaInis, colecionaveis = atualizaCol}

      where
--1 e 2
      {-| A função atualizaIni faz as seguintes alterações na lista de inimigos:
          - Se o jogador estiver armado e o inimigo colidir com a hitbox do martelo, então o inimigo será atingido e perde uma vida. Caso a sua vida chegue a 0, desaparece do mapa.
          - Se o bloco em que o inimigo está for Vazio e o bloco abaixo dele também, então o inimigo vai cair devido à gravidade.
      -}

      atualizaInis = map atualizaIni ini

            where
            atualizaIni ini | armado && t > 0 && colideMartelo ini && vidaFinal /= 0 = ini { vida = vidaFinal }
                            | armado && t > 0 && colideMartelo ini && vidaFinal == 0 = ini { vida = vidaFinal, posicao = (-1, -1) }
                            | verificaVazioIni ini = ini {posicao = (x1,y1 + g)}
                            | otherwise = ini
                  where
                  t = snd (aplicaDano jog)
                  vidaFinal = max 0 (vida ini - 1)
                  g = snd gravidade
                  (x1,y1) = posicao ini

            colideMartelo inimigo = case direcao jog of
                  Este  -> ((((x + larg/2) + larg) <= (x1 + larg1/2) )&& ((x + larg/2) >= (x1 - larg1/2))) && (((y - alt/2) >= (y1 - alt1/2) && (y-alt/2) <= (y1 + alt1/2) || (y + alt/2) <= (y1 + alt1/2) && (y + alt/2) >= (y1 - alt1/2)))
                  Oeste -> (((x - larg/2) >= (x1 - larg1/2)) && (((x - larg/2 )- larg) <= (x1 + larg1/2))) && (((y - alt/2) >= (y1 - alt1/2) && (y-alt/2) <= (y1 + alt1/2) || (y + alt/2) <= (y1 + alt1/2) && (y + alt/2) >= (y1 - alt1/2)))
                  _ -> False
                  where
                  (x1, y1) = posicao inimigo
                  (larg1, alt1) = tamanho inimigo
                  (x, y) = posicao jog
                  (larg, alt) = tamanho jog


            armado = case aplicaDano jog of
              (True, _) -> True
              _ -> False


-- 3
            verificaVazioIni ini = (blocos !! ceiling hy2 !! floor x1  == Vazio && blocos !! (ceiling hy2 + 1 )!! floor x1 == Vazio ) || (fromIntegral (ceiling y1) -alt/2)>y1 -- a primeira parte é para se ele passar pelo vazio e a segunda parte ( depois do ||) é para quando ele está a meio do salto
                  where
                  (x1,y1) = posicao ini
                  (larg, alt) = tamanho ini
                  g = snd gravidade
                  hb = ((hx1,hy1),(hx2,hy2))
                  hx1 = x1 - larg/2
                  hx2 = x1 + larg/2
                  hy1 = y1 + alt/2
                  hy2 = y1 - alt/2



--1, 3, 4 e 5
      {-| A função atualizaJog faz as seguintes alterações ao Jogador:
          - Se o aplicaDano for True, o seu tempo de dano decresce em função do tempo dado e se chegar a 0, passa a False.
          - Se o jogador estiver armado e for atingido pelas costas, perde uma vida.
          - Se o jogador não estiver armado e for atingido de qualquer lado, perde uma vida.
          - Se o jogador apanhar uma Moeda, ganha 10 pontos.
          - Se o jogador apanhar um Martelo, aplicaDano muda para "(True,10)".
          - Se o bloco em que o jogador está for vazio e o bloco abaxo também, então o jogador cai devido à gravidade.
          - Se o jogador estiver num bloco de escada, então emEscada = True, senão emEscada = False
      -}

      atualizaJog
            | fst (aplicaDano jog) = apDanoTempo jog temp
            | atingidoCostas = jog {vida = vidaAtualizada} -- ainda nao funciona
            | atingido = jog {vida = vidaAtualizada}
            | coletaQual col == Just Moeda = jog {pontos = pontosAtualizados}
            | coletaQual col == Just Martelo = jog {aplicaDano = apDanoAtualizado}
            | atualizaGraviadeJog = jog {posicao = (fst (posicao jog), snd (posicao jog) +g)}
            | emescada  = jog {emEscada = True}
            | not emescada = jog {emEscada = False}
            | velocidadeXMaior jog = jog {velocidade = (fst (velocidade jog)-0.5 ,snd (velocidade jog)), posicao = (fst (posicao jog)+0.5 , snd (posicao jog))}
            | velocidadeXMenor jog = jog {velocidade = (fst (velocidade jog)+0.5 ,snd (velocidade jog)), posicao = (fst (posicao jog)-0.5 , snd (posicao jog))}
            | velocidadeYMaior jog = jog {velocidade = (fst (velocidade jog) ,snd (velocidade jog)-0.5), posicao = (fst (posicao jog) , snd (posicao jog)+0.5)}
            | velocidadeYMenor jog = jog {velocidade = (fst (velocidade jog) ,snd (velocidade jog)+0.5), posicao = (fst (posicao jog) , snd (posicao jog)-0.5)}
            | otherwise = jog

            where
            vidaAtualizada = max 0 (vida jog - 1)
            pontosAtualizados = pontos jog + 10
            apDanoAtualizado  = (True,10)
            g = snd gravidade

            velocidadeXMaior:: Personagem -> Bool
            velocidadeXMaior jog = a > 0.5
                  where a = fst (velocidade jog)

            velocidadeXMenor :: Personagem -> Bool
            velocidadeXMenor jog = a <0.5
                  where a = fst (velocidade jog)

            velocidadeYMenor:: Personagem -> Bool
            velocidadeYMenor jog = b < 0.5
                  where b = snd (velocidade jog)

            velocidadeYMaior:: Personagem -> Bool
            velocidadeYMaior jog = b >0.5
                  where b = snd ( velocidade jog)

--1
            apDanoTempo :: Personagem -> Tempo -> Personagem
            apDanoTempo _ 0 = jog
            apDanoTempo jog t
                  | tempo0 = jog{aplicaDano = (False,0.0)} -- não está a funcionar
                  | fst (aplicaDano jog) = jog {aplicaDano = apDT}
                  | otherwise = jog

                   where
                  apDT = (True, max 0 (tempoDano - t))
                  tempoDano = snd (aplicaDano jog)
                  tempo0 = tempoDano <= 0.0 && fst (aplicaDano jog)


            emescada = emEscadat jog (Mapa (pi,di) pe blocos)
                  where
                  emEscadat :: Personagem -> Mapa -> Bool
                  emEscadat jog (Mapa (pi,di) pe blocos) =  blocos !! floor y !! floor x == Escada
                       where (x,y) = posicao jog


--5
            coletaQual :: [(Colecionavel, Posicao)] -> Maybe Colecionavel -- ^ Diz qual foi o item que colidiu
            coletaQual [] = Nothing
            coletaQual (col1:t) | colideCol (snd col1) = Just (fst col1)
                                | otherwise = coletaQual t
                  where
                  colideCol (xC,yC) = case dir of
                              Este  -> (x + larg/2) >= (xC - largC/2) && (x + larg/2) <= (xC + largC/2) && ((y - alt/2) >= (yC - altC/2) && (y-alt/2) <= (yC + altC/2) || (y + alt/2) <= (yC + altC/2) && (y + alt/2) >= (yC - altC/2))
                              Oeste -> (x - larg/2) >= (xC - largC/2) && (x - larg/2) <= (xC + largC/2) && ((y - alt/2) >= (yC - altC/2) && (y-alt/2) <= (yC + altC/2) || (y + alt/2) <= (yC + altC/2) && (y + alt/2) >= (yC - altC/2))
                              _ -> False

                  largC = 1
                  altC  = 1
                  (x, y) = posicao jog
                  (larg, alt) = tamanho jog
                  dir = direcao jog

--3
            atualizaGraviadeJog = (blocos !! ceiling y2 !! floor x  == Vazio && blocos !! (ceiling y2 + 1 )!! floor x == Vazio ) || (fromIntegral (ceiling y) -alt/2)>y -- a primeira parte é para se ele passar pelo vazio e a segunda parte ( depois do ||) é para quando ele está a meio do salto

                  where
                  (x,y) = posicao jog
                  (larg, alt) = tamanho jog
                  g = snd gravidade
                  hb = ((x1,y1),(x2,y2))
                  x1 = x - larg/2
                  x2 = x + larg/2
                  y1 = y + alt/2
                  y2 = y - alt/2

--4
            atingido = colideJogIni jog ini && not armado
                  where
                  colideJogIni :: Personagem -> [Personagem] -> Bool
                  colideJogIni jog [] = False
                  colideJogIni jog (ini1:t) = colisoesPersonagens jog ini1 && colideJogIni jog t

                  armado = case aplicaDano jog of
                        (True, _) -> True
                        _ -> False

--1
            atingidoCostas = colideCostasInis jog ini && armado
                  where
                  colideCostasInis :: Personagem -> [Personagem] -> Bool
                  colideCostasInis jog [] = False
                  colideCostasInis jog (ini1:t) = colideCostas jog ini1 || colideCostasInis jog t

                        where
                        colideCostas jog inimigo = case direcao jog of
                              Este  -> (((x + larg/2) >= (x1 - larg1/2) )&& ((x - larg/2) <= (x1 + larg1/2))) && (((y - alt/2) >= (y1 - alt1/2) && (y-alt/2) <= (y1 + alt1/2) || (y + alt/2) <= (y1 + alt1/2) && (y + alt/2) >= (y1 - alt1/2)))
                              Oeste -> (((x + larg/2) >= (x1 - larg1/2)) && ((x - larg/2) <= (x1 + larg1/2))) && (((y - alt/2) >= (y1 - alt1/2) && (y-alt/2) <= (y1 + alt1/2) || (y + alt/2) <= (y1 + alt1/2) && (y + alt/2) >= (y1 - alt1/2)))
                              _ -> False

                              where
                              (x1, y1) = posicao inimigo
                              (larg1, alt1) = tamanho inimigo
                              (x, y) = posicao jog
                              (larg, alt) = tamanho jog

                  armado = case aplicaDano jog of
                        (True, _) -> True
                        _ -> False




--5
      {-| A função atualizaCol faz as seguintes alterações à lista de colecionaveis:
          - Se o jogador coletar o colecionavel, então ele é removido da lista de colecionaveis.
      -}

      atualizaCol = removeCol col
            where
            removeCol col = filter (\x -> not (colideCol (snd x))) col
                  where
                  colideCol (xC,yC) = case dir of
                              Este  -> (x + larg/2) >= (xC - largC/2) && (x + larg/2) <= (xC + largC/2) && ((y - alt/2) >= (yC - altC/2) && (y-alt/2) <= (yC + altC/2) || (y + alt/2) <= (yC + altC/2) && (y + alt/2) >= (yC - altC/2))
                              Oeste -> (x - larg/2) >= (xC - largC/2) && (x - larg/2) <= (xC + largC/2) && ((y - alt/2) >= (yC - altC/2) && (y-alt/2) <= (yC + altC/2) || (y + alt/2) <= (yC + altC/2) && (y + alt/2) >= (yC - altC/2))
                              _ -> False

                  largC = 1
                  altC  = 1
                  (x, y) = posicao jog
                  (larg, alt) = tamanho jog
                  dir = direcao jog


--6
      {-| A função atualizaMapa faz as seguintes alterações ao mapa:
          - Se o jogador passar por cima de um Alçapão, o bloco passa a ser Vazio para que o jogador caia.
      -}

      atualizaMapa | pisaAlcapao = Mapa (pi,di) pe (substitui (floor y1,floor x1) Vazio blocos)
                   | otherwise = Mapa (pi,di) pe blocos
                  where
                  intervaloX = [x + y / 10 | x <- [0..fromIntegral (length (head blocos))], y <- [1, 2, 3, 4, 5]] -- ^ intervalo com valores valores x.1 .2 .3 .4 .5 para que y.5 estivesse sempre incluso
                  pisaAlcapao = if elem x intervaloX && ((x<x2) && (x2<x+larg)) -- ^ está no intervalo e o x vai até .4 do bloco à direita e .6 à esquerda
                       then blocos !! floor y1 !! floor x1 == Alcapao
                       else blocos !! floor y1 !! (floor x2 - 1 ) == Alcapao

                  hb = ((x1,y1),(x2,y2))
                  x1 = x - larg/2
                  x2 = x + larg/2
                  y1 = y + alt/2
                  y2 = y - alt/2
                  (larg,alt) = tamanho jog
                  (x, y) = posicao jog


                  substitui :: (Int, Int) -> a -> [[a]] -> [[a]] -- para sustituir o A por P
                  substitui (l, c) v blocos = listaAntes ++ [listaAtual] ++ listaDepois
                        where
                        listaAntes = take l blocos
                        listaAtual = take c (blocos !! l) ++ [v] ++ drop (c + 1) (blocos !! l)
                        listaDepois = drop (l + 1) blocos




