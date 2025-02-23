module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Bitmap
import LI12324
import Tarefa1
import Tarefa2
import Tarefa3
import Tarefa4
import Data.Maybe
import GHC.Float (float2Double)



type Images = [Picture]

data  MenuAtivo = Jogar | Sair deriving (Show,Eq)

data World = World {menuAtivo :: MenuAtivo,
                    jogo :: Jogo,
                    imagens :: Images,
                    atual :: String,
                    imagens2:: Imagens}




data Imagens = Imagens {blocos      :: ImagensBlocos,
                        personagens :: ImagensPersonagens,
                        coleci      :: ImagensColecionaveis
                       }

type ImagensBlocos = [(Bloco, Picture)]
type ImagensPersonagens = [(Entidade, Picture)]
type ImagensColecionaveis = [(Colecionavel,Picture)]

type MapaBlocos = [[Bloco]]

m1 :: Mapa
m1 = Mapa ((0.5,5.5), Oeste) (3.5,3.5) mapaBl

mapaBl :: MapaBlocos
mapaBl = [[Plataforma,  Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma],
          [Plataforma,  Vazio,      Vazio,      Vazio,      Vazio,      Vazio,      Vazio,      Vazio,      Vazio,      Plataforma],
          [Plataforma,  Vazio,      Vazio,      Plataforma, Plataforma, Plataforma, Plataforma, Vazio,      Vazio,      Plataforma],
          [Plataforma,  Vazio,      Vazio,      Escada,     Vazio,      Vazio,      Escada,     Vazio,      Vazio,      Plataforma],
          [Plataforma,  Vazio,      Vazio,      Escada,     Vazio,      Vazio,      Escada,     Vazio,      Vazio,      Plataforma],
          [Plataforma,  Vazio,      Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Vazio,      Plataforma],
          [Plataforma,  Vazio,      Escada,     Vazio,      Vazio,      Vazio,      Vazio,      Escada,     Vazio,      Plataforma],
          [Plataforma,  Vazio,      Escada,     Vazio,      Vazio,      Vazio,      Vazio,      Escada,     Vazio,      Plataforma],
          [Plataforma,  Plataforma, Plataforma, Plataforma, Alcapao,    Alcapao,    Plataforma, Plataforma, Plataforma, Plataforma],
          [Plataforma,  Escada,     Vazio,      Vazio,      Vazio,      Vazio,      Vazio,      Vazio,      Escada,     Plataforma],
          [Plataforma,  Escada,     Vazio,      Vazio,      Vazio,      Vazio,      Vazio,      Vazio,      Escada,     Plataforma],
          [Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]]


inim = [Personagem {velocidade = (1,1), posicao = (2.5,2.5), tipo = MacacoMalvado, vida = 1, tamanho = (1,1), direcao = Este, emEscada = False, ressalta = False, pontos = 2, aplicaDano = (False,0)}
       ,Personagem {velocidade = (1,1), posicao = (7.5,2.5), tipo = Fantasma,      vida = 1, tamanho = (1,1), direcao = Este, emEscada = False, ressalta = False, pontos = 2, aplicaDano = (False,0)}
       ,Personagem {velocidade = (1,1), posicao = (4.5,4.5), tipo = MacacoMalvado, vida = 2, tamanho = (1,1), direcao = Este, emEscada = False, ressalta = False, pontos = 2, aplicaDano = (False,0)}]
jogad = Personagem  {velocidade = (2,2), posicao = (4.5,10.5), tipo = Jogador,       vida = 2, tamanho = (1,1), direcao = Oeste, emEscada = True, ressalta = False, pontos = 2, aplicaDano = (False,0)}

colec = [(Martelo,(3.5,7.5)), (Martelo,(2.5,2.5)), (Moeda,(4.5,6))]

jogoInicial :: Jogo
jogoInicial = Jogo m1 inim colec jogad




desenhaEstado :: World -> Picture -- sitio dos butoes
desenhaEstado (World menu (Jogo m1 inim colec jogad) img a img2)
  = if a == "Menu"
    then mconcat
         [ scale 7.1 4 $ translate 1.1 1 (img !! 0)
         , scale 4.5 4 $ translate (-1) 10 (scaleImage Jogar (img !! 1))
         , scale 3.2 3.2 $ translate (-1) (-70) (scaleImage Sair (img !! 2))
         ]
    else Pictures $ desenhaMapa (blocoComIm mapaBl imbl) (0,0) ++ [jogPic] ++ [iniPic] ++ [colPic]
      where
      imbl = blToIm img2
      jogPic = desenhaJogador ((persIm img2) !! 0) (posicao jogad)
      iniPic = desenhaInimigos ((persIm img2) !! 1) (posicao (head inim))
      colPic = desenhaCol ((colIm img2) !! 0) (snd (head colec))
      scaleImage opcao img' = scale (if menu == opcao then 0.6 else 0.5) (if menu == opcao then 0.6 else 0.5) img'


estadoinicial :: Images -> Imagens -> World
estadoinicial img img2 = World Jogar jogoInicial img "Menu" img2



reageEvento :: Event -> World -> World
reageEvento (EventKey (SpecialKey KeyUp) Down _ _)    (World menu jogo img a img2) = World (opAntMenu menu) jogo img a img2
reageEvento (EventKey (SpecialKey KeyDown) Down _ _)  (World menu jogo img a img2) = World (opAntMenu menu) jogo img a img2
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (World Sair jogo img "Menu" img2) = error "O jogo foi encerrado"
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (World Jogar jogo img "Menu" img2) = World  Sair jogo img "Jogo" img2
reageEvento (EventKey (Char 'p') Down _ _)            (World Sair jogo img "Jogo" img2) = World Jogar jogo img "Menu" img2
reageEvento (EventKey (Char 'P') Down _ _)            (World Sair jogo img "Jogo" img2) = World Jogar jogo img "Menu" img2
reageEvento (EventKey (Char 'w') Down _ _)            (World jogar jogo img "Jogo" img2) = World jogar (atualiza [Nothing] (Just Subir)         jogo) img "Jogo" img2
reageEvento (EventKey (Char 'w') Up _ _)              (World jogar jogo img "Jogo" img2) = World jogar (atualiza [Nothing]  Nothing             jogo) img "Jogo" img2
reageEvento (EventKey (Char 'W') Down _ _)            (World jogar jogo img "Jogo" img2) = World jogar (atualiza [Nothing] (Just Subir)         jogo) img "Jogo" img2
reageEvento (EventKey (Char 'W') Up _ _)              (World jogar jogo img "Jogo" img2) = World jogar (atualiza [Nothing]  Nothing             jogo) img "Jogo" img2
reageEvento (EventKey (Char 's') Down _ _)            (World jogar jogo img "Jogo" img2) = World jogar (atualiza [Nothing] (Just Descer)        jogo) img "Jogo" img2
reageEvento (EventKey (Char 's') Up _ _)              (World jogar jogo img "Jogo" img2) = World jogar (atualiza [Nothing]  Nothing             jogo) img "Jogo" img2
reageEvento (EventKey (Char 'S') Down _ _)            (World jogar jogo img "Jogo" img2) = World jogar (atualiza [Nothing] (Just Descer)        jogo) img "Jogo" img2
reageEvento (EventKey (Char 'S') Up _ _)              (World jogar jogo img "Jogo" img2) = World jogar (atualiza [Nothing]  Nothing             jogo) img "Jogo" img2
reageEvento (EventKey (Char 'a') Down _ _)            (World jogar jogo img "Jogo" img2) = World jogar (atualiza [Nothing] (Just AndarEsquerda) jogo) img "Jogo" img2
reageEvento (EventKey (Char 'a') Up _ _)              (World jogar jogo img "Jogo" img2) = World jogar (atualiza [Nothing]  Nothing             jogo) img "Jogo" img2
reageEvento (EventKey (Char 'A') Down _ _)            (World jogar jogo img "Jogo" img2) = World jogar (atualiza [Nothing] (Just AndarEsquerda) jogo) img "Jogo" img2
reageEvento (EventKey (Char 'A') Up _ _)              (World jogar jogo img "Jogo" img2) = World jogar (atualiza [Nothing]  Nothing             jogo) img "Jogo" img2
reageEvento (EventKey (Char 'd') Down _ _)            (World jogar jogo img "Jogo" img2) = World jogar (atualiza [Nothing] (Just AndarDireita)  jogo) img "Jogo" img2
reageEvento (EventKey (Char 'd') Up _ _)              (World jogar jogo img "Jogo" img2) = World jogar (atualiza [Nothing]  Nothing             jogo) img "Jogo" img2
reageEvento (EventKey (Char 'D') Down _ _)            (World jogar jogo img "Jogo" img2) = World jogar (atualiza [Nothing] (Just AndarDireita)  jogo) img "Jogo" img2
reageEvento (EventKey (Char 'D') Up _ _)              (World jogar jogo img "Jogo" img2) = World jogar (atualiza [Nothing]  Nothing             jogo) img "Jogo" img2
reageEvento (EventKey (SpecialKey KeySpace) Down _ _) (World jogar jogo img "Jogo" img2) = World jogar (atualiza [Nothing] (Just Saltar)        jogo) img "Jogo" img2
reageEvento (EventKey (SpecialKey KeySpace) Up _ _)   (World jogar jogo img "Jogo" img2) = World jogar (atualiza [Nothing]  Nothing             jogo) img "Jogo" img2



reageEvento _ s = s



-- Por aqui a T4
reageTempo :: Float -> World -> World
reageTempo 60  (World menu jogo img a img2) = if a == "Jogo"
                                              then World {jogo = movimenta 1 (float2Double 60) jogoInicial}
                                              else World menu jogo img a img2
reageTempo _ world = world


proxOpMenu :: MenuAtivo -> MenuAtivo
proxOpMenu Jogar = Sair
proxOpMenu Sair = Jogar


opAntMenu :: MenuAtivo -> MenuAtivo
opAntMenu Jogar = Sair
opAntMenu Sair = Jogar

blocosPic :: (Bloco,Picture) -> Picture
blocosPic (Plataforma,p) = p
blocosPic (Escada,e) = e
blocosPic (Alcapao,a) = a
blocosPic (Vazio,_) = Blank


desenhaLinha :: [(Bloco,Picture)] -> Int -> [Picture]
desenhaLinha [] _ = []
desenhaLinha (bp:t) x = translate (fromIntegral (x*64)) 300 (blocosPic bp):desenhaLinha t (x+1)

desenhaMapa :: [[(Bloco,Picture)]] -> (Int,Int) -> [Picture]
desenhaMapa [] _ = []
desenhaMapa (bp:t) (x,y) = translate (-280) (fromIntegral (y*(-64))) (pictures (desenhaLinha bp x)):desenhaMapa t (x,y+1)


blocoPic :: Bloco -> ImagensBlocos -> (Bloco,Picture)
blocoPic Plataforma p = (Plataforma,snd (p !! 0))
blocoPic Escada e = (Escada,snd (e !! 1))
blocoPic Alcapao a = (Alcapao,snd (a !! 2))
blocoPic Vazio v = (Vazio, Blank)

blocoComIm  :: [[Bloco]] -> ImagensBlocos -> [[(Bloco,Picture)]]
blocoComIm [[]] _ = []
blocoComIm mpbl imbl = map (map (\x -> blocoPic  x imbl)) mpbl


blToIm :: Imagens -> ImagensBlocos
blToIm (Imagens {blocos = imbl}) = imbl

persIm :: Imagens -> [(Entidade,Picture)]
persIm (Imagens {personagens = [(Jogador, j), (Fantasma, f)]})
  = [(Jogador, j), (Fantasma, f)]

colIm :: Imagens -> [(Colecionavel,Picture)]
colIm (Imagens {coleci = [(Martelo, m)]}) = [(Martelo, m)]


desenhaJogador :: (Entidade,Picture) -> (Double, Double) -> Picture
desenhaJogador (Jogador,pic) (x,y) = translate (realToFrac (x*64) - 310) (realToFrac (y*(-64) + 330)) (pictures [pic])

desenhaInimigos :: (Entidade,Picture) -> (Double, Double) -> Picture
desenhaInimigos (Fantasma,pic) (x,y) = translate (realToFrac (x*64) - 310) (realToFrac (y*(-64) + 330)) (pictures [pic])
desenhaInimigos (MacacoMalvado,pic) (x,y) = translate (realToFrac (x*64) - 310) (realToFrac (y*(-64) + 330)) (pictures [pic])

desenhaCol :: (Colecionavel,Picture) -> (Double,Double) -> Picture
desenhaCol (Martelo,pic) (x,y) = translate (realToFrac (x*64) - 310) (realToFrac (y*(-64) + 330)) (pictures [pic])


loadImg :: IO Images
loadImg = do  fundo <- loadBMP "img/fundo.bmp"
              imagemJogar <- loadBMP "img/start5.bmp"
              imagemSair <- loadBMP "img/exit4.bmp"
              let fundos = [fundo, imagemJogar, imagemSair]
              return fundos

loadImg2 :: IO Imagens
loadImg2 = do p <- loadBMP "img/plat.bmp"
              e <- loadBMP "img/escada.bmp"
              a <- loadBMP "img/alc.bmp"
              j <- loadBMP "img/mush.bmp"
              f <- loadBMP "img/fantasma.bmp"
              m <- loadBMP "img/martelo.bmp"
              return $ Imagens {personagens = [(Jogador,j), (Fantasma,f)], blocos = [(Plataforma,p), (Escada,e), (Alcapao, a), (Vazio, Blank)], coleci = [(Martelo,m)]}

cordofundo :: Color
cordofundo = makeColorI 50 50 50 255

main :: IO ()
main = do
  putStrLn "Carregando imagens..."
  fundos <- loadImg
  img2 <- loadImg2
  putStrLn "Imagens carregadas"
  play
    (InWindow "Projeto" (1920, 1080) (0, 0))
    cordofundo
    60
    (estadoinicial fundos img2)
    desenhaEstado
    reageEvento
    reageTempo