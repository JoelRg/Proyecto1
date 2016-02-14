import System.Random

rcard = do
  g <- newStdGen
  print $ fst (randomR ('a', 'z') g)


  
data   Card = Card {
       val :: Value,
       suit :: Suit
}

instance Show Card where 
	show (Card v s )      =    (show s) ++ (show v)



data Suit =  Clubs | Diamonds | Spades | Hearts deriving (Eq) 

--- Hay que colocar los caracteres especiales, en windows no sirve --

instance Show Suit where 
	show Clubs      =    "C"
	show Diamonds   =    "D"
	show Spades     =    "S"		 
	show Hearts     =    "H"


data Value = Numeric Int | Jack | Queen | King | Ace deriving (Eq) 


instance Show Value where 
	show (Numeric a)        =    (show a)
	show Jack               =    "J"
	show Queen              =    "Q"		 
	show King               =    "K" 
	show Ace                =    "A"

newtype Hand = H [Card] 

instance Show Hand where 
	show (H [])     =    " \n"  
	show (H [x])    =    (show x)
	show (H (x:xs)) =    (show x) ++ " " ++ (show (H (xs)))

empty :: Hand
empty = H []

addCard :: Card -> Hand -> Hand
addCard y (H x) = (H (y:x))

size :: Hand -> Int
size (H x) = length x

value :: Hand -> Int
value (H x)    = if sum (map valCard x) > 21 then sum (map valLow x)
	else sum (map valCard x)

valCard :: Card -> Int
valCard (Card (Numeric i) _) = i
valCard (Card  Ace _       )  = 11
valCard (Card  _   _)         = 10 

valLow :: Card -> Int
valLow (Card (Numeric i) _) = i
valLow (Card  Ace _       )  = 1
valLow (Card  _   _)         = 10

data Player = LambdaJack | You deriving (Show)

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
	    y = map nAdd [(a, b)| a <- [Clubs, Spades, Diamonds, Hearts], b <- (take 10 (iterate (+1) 1))]
	in H (x ++ y)

--- nAdd se encarga de crear una Carta dado un Suit y un Int --------

nAdd :: (Suit,Int) -> Card
nAdd (x,a) = (Card (Numeric a) x)

--- sAdd se encarga de agregar los representantes no numericos ------
--- de un suit. -----------------------------------------------------

sAdd :: Suit -> [Card]
sAdd x = [(Card Jack x),(Card Queen x),(Card King x),(Card Ace x)]

--- shuffle :: stdGen -> Hand -> Hand