
{-  Verificador de Tautologías  -}

import Data.List (nub)  -- Eliminar Repetidos 
import Data.Maybe

--  Tipo de expresiones de Lógica Proposicional.

data Proposition = Const Bool               -- Constante Booleana.
          | Var String                      -- Una variable, representada por un String.
          | Neg Proposition                 -- La negación de una expresion de Lógica Propositionosicional.
          | Conj Proposition Proposition    -- La conjunción de dos expresiones de Lógica Propositionosicional.
          | Disj Proposition Proposition    -- La disyunción de dos expresiones de Lógica Propositionosicional.
          | Impl Proposition Proposition    -- La implicación de dos expresiones de Lógica Propositionosicional.
            deriving (Eq,Show)


--  Ambiente de Evaluación.

type Environment = [(String,Bool)] 


--  Esta funcion determina si la variable k está definida en el ambiente e, 
--  produciendo su valor booleano en caso afirmativo.

find :: Environment -> String -> Maybe Bool
find [] _ = Nothing
find (x:xs) k
      | (fst x) == k = Just (snd x)
      | otherwise = find xs k  


--  Esta funcion es tal que: 
--    Si en el ambiente e no existe ninguna asociación para la variable k, la función
--  produce un nuevo ambiente igual al original pero agregando la asociación (k,v)
--  al principio.
--    Si en el ambiente e ya existe una asociación para la variable k, la función produce
--  un nuevo ambiente igual al original reemplazando la asociación existente por la
--  nueva.

addOrReplace :: Environment -> String -> Bool -> Environment
addOrReplace e k v 
      | isNothing (find e k) = (k,v):e
      | otherwise = replace e [] k v 
      where replace [] e' _ _ = e'
            replace (x:xs) e' k v 
              | (fst x) == k = replace xs ((k,v):e') k v
              | otherwise = replace xs (x:e') k v


--  Esta funcion es tal que: 
--    Si en el ambiente e no existe ninguna asociación para la variable k, la función
--  produce el mismo ambiente sin modificar.
--    Si en el ambiente e existe una asociación para la variable k, la función produce
--  un nuevo ambiente igual al original eliminando la asociación existente.


remove :: Environment -> String -> Environment
remove e k 
      | isNothing (find e k) = e
      | otherwise = auxremove e [] k 
      where auxremove [] e' _ = e'
            auxremove (x:xs) e' k 
              | (fst x) == k = auxremove xs e' k 
              | otherwise = auxremove xs (x:e') k 


--  Esta funcion recorre la estructura recursiva de Proposition y se apoya en el 
--  ambiente e para determinar si la proposición p tiene un valor de verdad, calculándolo.

evalP :: Environment -> Proposition -> Maybe Bool
evalP _ (Const x)  = Just x
evalP e (Var k)    = find e k
evalP e (Neg p)    = Just (not(fromJust (evalP e p)))
evalP e (Conj p q) = Just (fromJust (evalP e p) && fromJust (evalP e q))
evalP e (Disj p q) = Just (fromJust (evalP e p) || fromJust (evalP e q))
evalP e (Impl p q) = Just (not(fromJust (evalP e p)) || (fromJust (evalP e q)))


--  Esta función extrae los nombres de todas las variables presentes en la 
--  proposición p. Si una variable aparece más de una vez en la proposición, debe 
--  aparecer una sola vez en la lista.

vars :: Proposition -> [String]
vars p = nub (auxvars p)
  where auxvars (Const _)   =  []
        auxvars (Var x)     =  [x]
        auxvars (Neg p)     =  auxvars p
        auxvars (Conj p q)  =  auxvars p ++ auxvars q
        auxvars (Disj p q)  =  auxvars p ++ auxvars q
        auxvars (Impl p q)  =  auxvars p ++ auxvars q


-- Esta función genera todas las secuencias de n Booleanos.

sequences :: Int -> [[Bool]]
sequences 0 = [[]]
sequences n = [b:bs | bs<-sequences (n-1), b<-[False,True]]

--  Esta función determina todas las asignaciones a las variables de una Proposicion. 

assignments :: Proposition -> [Environment]
assignments p = map (zip vs) (sequences (length vs))
    where vs = vars p

--  Esta función determina si la proposición p es una Tautología. El algoritmo 
--  general consiste en generar todos los Ambientes de Evaluación resultantes de asignar 
--  todas las combinaciones de valores de verdad a las variables presentes en la 
--  proposición, evaluarla, y determinar si siempre tiene valor de verdad True.

isTautology :: Proposition -> Bool
isTautology p = and [fromJust(evalP e p) | e<-assignments p]

