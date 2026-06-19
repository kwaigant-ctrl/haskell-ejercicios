import GHC.Windows (BOOL)
-- Armar lista de compras
-- Agregar alimentos que hay que comprar
-- saber cuantas cosas hay que comprar
-- saber si ya me anote un alimanto de la lista

type Alimento = String

agregarAlimentoALista :: Alimento -> [Alimento] -> [Alimento]
agregarAlimentoALista alimento listaDeCompras = alimento : listaDeCompras 

cantidadDeAlimentos :: [Alimento] -> Int
cantidadDeAlimentos {-listaDeCompras-} = length {-listaDeCompras-}  --borro lo redundante

yaEstaEnLaLista :: Alimento -> [Alimento] -> Bool
yaEstaEnLaLista alimento listaDeCompras = alimento `elem` listaDeCompras

-- generalizar una funcion todosCumplen

todosPares :: [Int] -> Bool
todosPares [] = True 
todosPares (x:xs) = even x && todosPares xs 

{- todosPares = foldr (\x acum -> even x && acum) True
-}

type Notas = Int

todosAprobados :: [Notas] -> Bool
todosAprobados [] = True
todosAprobados (x:xs) = (>= 6) x && todosAprobados xs

-- Version con orden superior

todosCumplen :: (a -> Bool) ->  [a] -> Bool
todosCumplen condicion [] = True
todosCumplen condicion (x:xs)  = condicion x && todosCumplen condicion xs

todosAprobados' :: [Notas] -> Bool
todosAprobados' {-lista-} = todosCumplen (>=6) {-lista-}

 