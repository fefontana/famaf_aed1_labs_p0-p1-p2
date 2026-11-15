{-# LANGUAGE NPlusKPatterns #-}

-- a) Definir el tipo Video que consta de dos constructores Pelicula y CapSerie con los siguientes par ́ametros:

-- Pelicula debe tomar como parametros el nombre, el director, la duracion (en minutos) y el ano de estreno. 
-- El constructor CapSerie debe tomar como par ́ametros el nombre de la serie, el nro de cap ́ıtulo, la temporada, el a ̃no de estreno de la temporada.


type Nombre = String
type Director = String
type Duracion = Int
type Estreno = Int
type NroCapitulo = Int
type NroTemporada = Int
data Video = Pelicula Nombre Director Duracion Estreno  
             | CapSerie Nombre NroCapitulo NroTemporada Estreno 
             deriving Show

elPadrino :: Video
elPadrino = Pelicula "El Padrino" "Francis Ford Coppola" 177 1972

breakingBadS01E01 :: Video
breakingBadS01E01 = CapSerie "Breaking Bad" 1 1 2008


-- c) Definir la funcion esPrimerCapitulo :: Video -> Bool que dado un Video, devuelve True si el video es el primer capıtulo de la primera temporada de una serie, False caso contrario.

esPrimerCapitulo :: Video -> Bool
esPrimerCapitulo (CapSerie n 1 1 e) = True
esPrimerCapitulo v = False

-- d) Definir la funcion esEstreno :: Video -> Bool que dado un Video, devuelve True si el video es una pelıcula cuyo ano de estreno es igual a 2024.

esEstreno :: Video -> Bool
esEstreno (Pelicula n d t 2024) = True
esEstreno v = False


-- e) Definir la funcion duracionPeliMasLarga :: [Video] -> Int que dada una lista de de videos, devuelve la duracion de la pelıcula mas larga. En caso que no haya películas devuelve 0.

duracionPeliMasLarga :: [Video] -> Int
duracionPeliMasLarga [] = 0
duracionPeliMasLarga ((Pelicula n d t e):xs) = max t (duracionPeliMasLarga xs)
duracionPeliMasLarga (v:xs) = duracionPeliMasLarga xs


duracionPeliMasLarga' :: [Video] -> Int
duracionPeliMasLarga' [] = 0
duracionPeliMasLarga' ((Pelicula _ _ t _):xs) | (t > duracionPeliMasLarga' xs) = t
                                              | otherwise = duracionPeliMasLarga' xs
duracionPeliMasLarga' (v:xs) = duracionPeliMasLarga' xs




-- maximo con lista vacía igual a 0
maximo' :: [Int] -> Int
maximo' [] = 0 
maximo' (x:xs) = max x (maximo' xs)

maximo :: [Int] -> Int
maximo [x] = x
maximo (x:xs) = max x (maximo xs)





-------------------------------------

factorial :: Int -> Int
factorial 0 = 1
factorial (n+1) = (n+1) * factorial n



data Natural = Cero | Suc Natural deriving Show

dos :: Natural
dos = Suc (Suc (Cero))


--fact :: Natural -> Natural
--fact Cero = Suc Cero
--fact (Suc n) = (Suc n) * fact n


data ListaInt = Vacia | Peg Int ListaInt deriving Show

-- l1 es la lista [1,2,7]
l1 = Peg 1 (Peg 2 ( Peg 7 Vacia))

sumListaInt :: ListaInt -> Int
sumListaInt Vacia = 0
sumListaInt (Peg x xs) = x + sumListaInt xs 



------------------------

-- f) Dado el tipo recursivo ColaVideo definido de la siguiente manera

data ColaVideo = Vacio | Encolada Video ColaVideo deriving Show

colaReproduccion :: ColaVideo
colaReproduccion = Encolada elPadrino ( Encolada breakingBadS01E01 Vacio )

-- Definir la funcion pelisDelDirector :: ColaVideo -> String -> ColaVideo que dada una cola de reproduccion de videos q, y el nombre de un director d, devuelve la cola de reproduccion que tiene solamente las pelıculas del director d (en el mismo orden que aparecen en q).

pelisDelDirector :: ColaVideo -> String -> ColaVideo
pelisDelDirector Vacio d = Vacio
pelisDelDirector (Encolada (Pelicula n d t e) vs) nd 
              | d == nd = Encolada (Pelicula n d t e) (pelisDelDirector vs nd)
              | otherwise = pelisDelDirector vs nd
pelisDelDirector (Encolada v vs) nd  = pelisDelDirector vs nd








