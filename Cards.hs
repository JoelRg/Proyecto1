module Cards where
import System.Random
import Data.List

--- Modulo Cards ----------------------------------------------------
--- Joel Rivas 11-10866 ---------------------------------------------
--- Larry Perez 10-10547 --------------------------------------------


--- Datos de la carta ----------------------------------------------- 
 
data   Card = Card {
       val :: Value,
       suit :: Suit
}  deriving (Eq)

instance Show Card where 
	show (Card v s )      =    (show s) ++ (show v)

--- Pinta de la carta -----------------------------------------------

data Suit =  Clubs | Diamonds | Spades | Hearts deriving (Eq) 

--- Hay que colocar los caracteres especiales, en windows no sirve 

instance Show Suit where 
	show Clubs      =    "♣"
	show Diamonds   =    "♦"
	show Spades     =    "♠" 
	show Hearts     =    "♥"

--- Valor de la carta -----------------------------------------------

data Value = Numeric Int | Jack | Queen | King | Ace deriving (Eq) 


instance Show Value where 
	show (Numeric a)        =    (show a)
	show Jack               =    "J"
	show Queen              =    "Q"		 
	show King               =    "K" 
	show Ace                =    "A"

--- Mano (Hand) -----------------------------------------------------

newtype Hand = H [Card] 

instance Show Hand where 
	show (H [])     =    "vacia"  
	show (H [x])    =    (show x)
	show (H (x:xs)) =    (show x) ++ " " ++ (show (H (xs)))

--- empty devuelve una mano vacia. ----------------------------------
	
empty :: Hand
empty = H []

--- addCard agrega una carta a la mano. -----------------------------

addCard :: Card -> Hand -> Hand
addCard y (H x) = (H (y:x))

size :: Hand -> Int
size (H x) = length x

value :: Hand -> Int
value (H x)    = if sum (map valCard x) > 21 then sum (map valLow x)
	else sum (map valCard x)

valCard :: Card -> Int
valCard (Card (Numeric i) _)  = i
valCard (Card  Ace _       )  = 11
valCard (Card  _   _)         = 10 

valLow :: Card -> Int
valLow (Card (Numeric i) _)   = i
valLow (Card  Ace _       )   = 1
valLow (Card  _   _)          = 10

--- Tipo Player -----------------------------------------------------

data Player = LambdaJack | You deriving (Show, Eq)

busted :: Hand -> Bool
busted (H x) =  sum (map valLow x) > 21 

--- Aqui asumimos que la primera mano SIEMPRE es de Lambda ----------

winner :: Hand -> Hand -> Player
winner (H x) (H y) = if value (H y) > 21 then LambdaJack 
	else if value (H x) > 21 then You 
		else if value (H y) > value (H x) then  You
			else LambdaJack

fullDeck :: Hand
fullDeck =
	let x = concatMap sAdd [ c | c <-  [Clubs, Spades, Diamonds, Hearts]]
	    y = map nAdd [(a, b)| a <- [Clubs,Spades,Diamonds,Hearts], b <- [1..10]]
	in H (x ++ y)

--- nAdd se encarga de crear una Carta dado un Suit y un Int --------

nAdd :: (Suit,Int) -> Card
nAdd (x,a) = (Card (Numeric a) x)

--- sAdd se encarga de agregar los representantes no numericos ------
--- de un suit. -----------------------------------------------------

sAdd :: Suit -> [Card]
sAdd x = [(Card Jack x),(Card Queen x),(Card King x),(Card Ace x)]

deletAt :: Int -> [a] -> [a]
deletAt _ [] = error "Lista vacia"
deletAt  0 (x:xs)= xs
deletAt  n (x:xs) = x: (deletAt (n-1) xs)

--- Dado un generador y un numero n devuelve un numero entre el -----
--- rango (0,n-1). --------------------------------------------------


tt :: [a] -> a
tt [] = error "Lista vacia"
tt (x:xs) = x

--- Procedimiento shuffle. ------------------------------------------

shuffle :: (RandomGen stdGen) => stdGen -> Hand -> Hand
shuffle r (H y) = H (shufList  r y )

shufList :: (RandomGen stdGen) => stdGen -> [Card] -> [Card]
shufList a b  = foldRand a (:) [] b

--- foldRand aplica una funcion de manera "aleatoria" sobre una -----
--- lista gracias a una semilla. ------------------------------------

foldRand :: RandomGen g => g -> (t -> b -> b) -> b -> [t] -> b
foldRand siid  f z []  = z
foldRand siid  f z x   = 
	let q = (x!!(tt(take 1 (randomRs (0 ,length(x)-1) siid ))))
	    r = (deletAt  (tt( take 1 (randomRs (0 ,length(x)-1) siid ))) x)
	in q `f` foldRand  siid f z r

--- draw devuelve una tupla con el mazo y la mano con una carta -----
--- adicional, robada del mismo, si se puede. sino, Nothing. --------

draw :: Hand -> Hand -> Maybe (Hand,Hand)
draw (H []) (H y) = Nothing
draw (H (x:xs)) (H y) = Just ((H xs),(H (x:y)))


	
	

	
