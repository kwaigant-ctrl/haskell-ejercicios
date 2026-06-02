{-
Aprenda a jugar golf con Lee Carvallo

Lisa Simpson se propuso desarrollar un programa que le permita ayudar a 
su hermano a vencer a su vecino Todd en un torneo de minigolf. Para 
hacerlo más interesante, los padres de los niños hicieron una apuesta: 
el padre del que no gane deberá cortar el césped del otro usando un 
vestido de su esposa.

De los participantes nos interesará el nombre del jugador, el de su 
padre y sus habilidades (fuerza y precisión).

- Modelo inicial
-}

data Jugador = UnJugador {
    nombre :: String,
    padre :: String,
    habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
    fuerzaJugador :: Int,
    precisionJugador :: Int
} deriving (Eq, Show)

-- Jugadores de ejemplo

bart :: Jugador
bart = UnJugador "Bart" "Homero" (Habilidad 25 60)

todd :: Jugador
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)

rafa :: Jugador
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)


data Tiro = UnTiro {
    velocidad :: Int,
    precision :: Int,
    altura :: Int
} deriving (Eq, Show)


type Puntos = Int

-- Funciones útiles

between :: (Eq a, Enum a) => a -> a -> a -> Bool
between n m x = elem x [n .. m]

maximoSegun :: (Foldable t, Ord a1) => (a2 -> a1) -> t a2 -> a2
maximoSegun f = foldl1 (mayorSegun f)

mayorSegun :: Ord a => (t -> a) -> t -> t -> t
mayorSegun f a b
  | f a > f b = a
  | otherwise = b

{-
También necesitaremos modelar los palos de golf que pueden usarse y los 
obstáculos que deben enfrentar para ganar el juego.

Sabemos que cada palo genera un efecto diferente, por lo tanto elegir el palo 
correcto puede ser la diferencia entre ganar o perder el torneo.
Modelar los palos usados en el juego que a partir de una determinada habilidad 
generan un tiro que se compone por velocidad, precisión y altura.

El putter genera un tiro con velocidad igual a 10, el doble de la precisión 
recibida y altura 0.

La madera genera uno de velocidad igual a 100, altura igual a 5 y la mitad de 
la precisión.

Los hierros, que varían del 1 al 10 (número al que denominaremos n), generan
un tiro de velocidad igual a la fuerza multiplicada por n, la precisión 
dividida por n y una altura de n-3 (con mínimo 0). 

Modelarlos de la forma más genérica posible.

Definir una constante palos que sea una lista con todos los palos que se pueden
usar en el juego.
Definir la función golpe que dados una persona y un palo, obtiene el tiro 
resultante de usar ese palo con las habilidades de la persona.

Por ejemplo si Bart usa un putter, se genera un tiro de 
velocidad = 10, precisión = 120 y altura = 0.
-}


-- palos

type Palo = Habilidad -> Tiro -- dada una habilidad, produce un tiro

--El putter genera un tiro con velocidad igual a 10, el doble de la precisión 
--recibida y altura 0.

putter :: Palo
putter habilidadJugador = UnTiro
    { velocidad = 10
    , precision = precisionJugador habilidadJugador * 2
    , altura = 0 
    }

-- La madera genera uno de velocidad igual a 100, altura igual a 5 y la mitad 
-- de la precisión.

madera :: Palo
madera habilidadJugador = UnTiro 
    { velocidad = 100
    , precision = div (precisionJugador habilidadJugador) 2
    , altura = 5 
    }

-- Los hierros, que varían del 1 al 10 (número al que denominaremos n), 
-- generan un tiro de velocidad igual a la fuerza multiplicada por n, la 
-- precisión dividida por n y una altura de n-3 (con mínimo 0). 

hierros :: Int -> Palo              -- no es un palo, es una funcion que recibe un entero y devuelve un palo
hierros n  habilidadJugador = UnTiro 
    { velocidad = (fuerzaJugador habilidadJugador) * n
    , precision = div (precisionJugador habilidadJugador) n
    , altura = n - 3
    }


{-Definir una constante palos que sea una lista con todos los palos que se pueden
usar en el juego.

Definir la función golpe que dados una persona y un palo, 
obtiene el tiro resultante de usar ese palo con las 
habilidades de la persona.
-}

-- DEFINIR UNA CONSTANTE
palos :: [Palo]
palos = putter : madera : map hierros [1..10] --[putter, madera, hierro1, hierro2...]

golpe :: Jugador -> Palo -> Tiro --tipos (ej: bart, hierro 3)
golpe persona palo = palo (habilidad persona)

-- OBSTACULOS

{-Lo que nos interesa de los distintos obstáculos es si un 
tiro puede superarlo, y en el caso de poder superarlo, cómo 
se ve afectado dicho tiro por el obstáculo. En principio 
necesitamos representar los siguientes obstáculos:

Un túnel con rampita sólo es superado si la precisión es mayor
a 90 yendo al ras del suelo, independientemente de la 
velocidad del tiro. Al salir del túnel la velocidad del tiro 
se duplica, la precisión pasa a ser 100 y la altura 0.
-}

type Obstaculo = Tiro -> Tiro

tiroDetenido:: Tiro
tiroDetenido = UnTiro 0 0 0 

tunelConRampita :: Obstaculo
tunelConRampita tiro 
    | precision tiro > 90 && altura tiro == 0 =
        UnTiro {velocidad= velocidad tiro * 2, precision=100, altura=0}
    | otherwise = tiroDetenido

{-
Una laguna es superada si la velocidad del tiro es mayor a 80 
y tiene una altura de entre 1 y 5 metros. Luego de superar una
laguna el tiro llega con la misma velocidad y precisión, pero 
una altura equivalente a la altura original dividida por el 
largo de la laguna.
-}

superarLaguna :: Int -> Obstaculo
superarLaguna largoLaguna tiro
    | velocidad tiro > 80 && altura tiro >= 1 && altura tiro <=5 =
        UnTiro 
        { velocidad = velocidad tiro
        , precision = precision tiro
        , altura= div (altura tiro) largoLaguna }
    | otherwise = tiroDetenido                   -- cuando no sirve

{-
Un hoyo se supera si la velocidad del tiro está entre 5 y 20 
m/s yendo al ras del suelo con una precisión mayor a 95. 
Al superar el hoyo, el tiro se detiene, quedando con todos sus 
componentes en 0. 

Se desea saber cómo queda un tiro luego de 
intentar superar un obstáculo, teniendo en cuenta que en caso 
de no superarlo, se detiene, quedando con todos sus 
componentes en 0.
-}

superarHoyo :: Obstaculo
superarHoyo tiro 
    | velocidad tiro >= 5 && velocidad tiro <= 20 && altura == 0 && precision > 95 =
        tiroDetenido

{-
Definir palosUtiles que dada una persona y un obstáculo, 
permita determinar qué palos le sirven para superarlo.

Saber, a partir de un conjunto de obstáculos y un tiro, 
cuántos obstáculos consecutivos se pueden superar.

Por ejemplo, para un tiro de velocidad = 10, precisión = 95 y 
altura = 0, y una lista con dos túneles con rampita seguidos 
de un hoyo, el resultado sería 2 ya que la velocidad al salir 
del segundo túnel es de 40, por ende no supera el hoyo.
BONUS: resolver este problema sin recursividad, teniendo en 
cuenta que existe una función 
takeWhile :: (a -> Bool) -> [a] -> [a] que podría ser de 
utilidad.
-}

palosUtiles :: Jugador -> Obstaculo -> [Palo] -- golpe va de jugador -> palo -> tiro    obtactulo de tiro -> tiro
palosUtiles jugador obstaculo = filter (leSirve jugador obstaculo) palos 

leSirve :: Jugador -> Obstaculo -> Palo -> Bool
leSirve jugador obstaculo palo =
    obstaculo (golpe jugador palo) /= tiroDetenido

{-
Definir paloMasUtil que recibe una persona y una lista de 
obstáculos y determina cuál es el palo que le permite superar 
más obstáculos con un solo tiro.
Dada una lista de tipo [(Jugador, Puntos)] que tiene la 
información de cuántos puntos ganó cada niño al finalizar 
el torneo, se pide retornar la lista de padres que pierden 
la apuesta por ser el “padre del niño que no ganó”. Se dice 
que un niño ganó el torneo si tiene más puntos que los otros 
niños.-}
