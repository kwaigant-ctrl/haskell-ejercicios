import qualified Control.Applicative as Float
{-
Armar consultas para saber:
- De entre los alimentos que son poco caloricos, si hay alguno que 
tengas mas proteina que grasas

- Que alimento tiene mayor valor calorico, mas carbohidratos, mayor 
nombre teniendo en cuenta que ya tenemos la funcion elDeMayor
-}

type Alimento = String

data InformacionNuticional = Info {
    alimento :: Alimento,          -- se usan como
    calorias :: Int,
    proteina :: Float,
    grasas :: Float,
    carbohidratos :: Float,
    fibra :: Float
}

-- falta definir valores

elDeMayor :: Ord b => (a -> b) -> a -> a -> a 
elDeMayor ponderacion a b                    -- cuando uso guardas no pongo =
    | ponderacion a > ponderacion b = a 
    | otherwise = b


-- buscar los poco caloricos

pocoCalorico :: InformacionNuticional -> Bool
pocoCalorico = (<=100).calorias

noPococalorico :: InformacionNuticional -> Bool
noPococalorico = not.pocoCalorico

-- ver si tienen mas proteina que grasa 

masProteinaQueGrasa :: InformacionNuticional -> Bool
masProteinaQueGrasa info = proteina info > grasas info

-- llamar a la funcion anterior para ver si existe alguno que tenga mas 
-- proteina que grasa 

hayPocoCaloricoConMasProteinaQueGrasa :: [InformacionNuticional] -> Bool
hayPocoCaloricoConMasProteinaQueGrasa =
    any masProteinaQueGrasa . filter pocoCalorico
    
--any (\info -> proteina info > grasas info) . filter (not.pocoCalorico) $ info

-- Que alimento tiene mayor valor calorico, mas carbohidratos, mayor 
-- nombre teniendo en cuenta que ya tenemos la funcion elDeMayor

-- busco cual es el de mayor calorias y despues busco el nombre del alimento

mayorValorCalorico :: [InformacionNuticional] -> Alimento
mayorValorCalorico = alimento . foldr1 (elDeMayor calorias)

mayorNombre :: [InformacionNuticional] -> Alimento
mayorNombre = alimento . foldr1 (elDeMayor alimento)

mayorCarbohidratos :: [InformacionNuticional] -> Alimento
mayorCarbohidratos = alimento . foldr1 (elDeMayor carbohidratos)

