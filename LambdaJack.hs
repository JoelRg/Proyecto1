import System.Random
import Data.List

--- BIENVENIDO A LAMBDAJACK -----------------------------------------
--- Joel Hernandez __-_____ -----------------------------------------
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
	show Clubs      =    "C"
	show Diamonds   =    "D"
	show Spades     =    "S"		 
	show Hearts     =    "H"

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

--- Jugadas de Lambda -----------------------------------------------

playLambda :: Hand -> IO Hand
playLambda (H []) = return $ (H [])
playLambda (H x) =  return $ drawAux (H x) (H [])

drawAux :: Hand -> Hand -> Hand
drawAux (H []) (H y) = (H y) 
drawAux (H x)  (H y) = 
	let val = draw (H x)  (H y)
	in if value (sM val) > 15 then
		(sM val) else
		drawAux (fM val ) (sM  val )
		
fM :: Maybe (Hand,Hand) -> Hand
fM Nothing = (H [])
fM (Just (x,y)) = x

sM :: Maybe (Hand,Hand) -> Hand
sM Nothing = (H [])
sM (Just (x,y)) = y

--- Estado del juego ------------------------------------------------

data GameState = GS {
games :: Int,
lambdaWins :: Int,
name :: String,
generator :: StdGen
}

welcome :: IO String 
welcome = do
	putStrLn "Bienvenido a LambdaJack"
	putStr "Escriba su nombre: "
	w <- getLine
	return w
	
currentState :: GameState -> IO()
currentState (GS g l n gen) = do
	putStr "Después de "
	putStr $ show g
	putStr " partidas Lambda ha ganado "
	putStr $ show l
	putStr " y "
	putStr n 
	putStr " ha ganado "
	print $ g-l
	
---revisar en linux
	
continuePlaying :: IO Bool
continuePlaying = do
	putStr "¿Desea seguir jugando? (y/n): "
	c <-getChar
	if c == 'y' then 
		return True
		else if c == 'n' then
			return False
			else do putStrLn "Caracter invalido, responda con 'y' (yes) o 'n' (no)"
				continuePlaying

playerHand :: Hand -> String -> IO()
playerHand h s = do
	putStr s
	putStr ", tu mano es "
	putStr $ show h
	putStr ", suma "
	putStr $ show $ value $ h
	putStr "."

lambdaHand :: Hand -> IO()
lambdaHand h = do
	putStr "Mi mano es "
	putStr $ show h
	putStr ", suma "
	putStr $ show $ value $ h
	putStr "."

drawCycle :: Hand -> Hand -> String -> IO (Hand,Hand)
drawCycle d h s = 
	let  j = "Caracter invalido, responda con 'c' (carta) o 'l' (listo)"
	     val = draw d h
	in do
		playerHand h s
		if busted h then do
			putStrLn " Perdiste."
			return (d,h)
			else do 
				putStrLn " ¿Carta o Listo?"
				c <- getChar
				if c == 'c' then
					drawCycle (fM val) (sM val) s
					else if c == 'l' then do
						putStrLn "Mi turno."
						return (d,h)
						else do 
							putStrLn j
							drawCycle d h s

gameloop :: GameState -> IO ()
gameloop (GS games lw name gen ) = 
	let deck = shuffle gen fullDeck
	in do
		ngen <- newStdGen 
		pg <- drawCycle deck (H []) name
		x <- playLambda ( fst ( pg))
		lambdaHand x
		if winner ( x) ( snd ( pg)) == You 
		then do
			putStrLn "Tu ganas."
			currentState (GS (games+1) lw name gen )
			cont <- continuePlaying
			if cont then do
				gameloop (GS (games+1) lw name ngen )
				else do
					putStrLn "Juego terminado."
			else do
				if value x == value ( snd ( pg)) then do
					putStrLn " Empatamos, asi que yo gano."
					else if (value x >= value ( snd ( pg))) && (value x > 21) then do
						putStrLn " Empatamos, asi que yo gano."
						else do
							putStrLn " Yo gano."
				currentState (GS (games+1) (lw+1) name gen )
				cont <- continuePlaying
				if cont then do
					gameloop (GS (games+1) (lw+1) name ngen )
					else do
						putStrLn "Juego terminado."
	
main :: IO ()
main = do
	s <- welcome
	gen <- newStdGen
	gameloop (GS 0 0 s gen)

	
	

	
