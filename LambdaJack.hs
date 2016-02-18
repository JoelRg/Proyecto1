module LambdaJack where
import System.Random
import Data.List
import Cards

--- Modulo LambdaJack -----------------------------------------------
--- Joel Rivas 11-10866 ---------------------------------------------
--- Larry Perez 10-10547 --------------------------------------------

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
	cPlaying

cPlaying :: IO Bool
cPlaying = do
	c <-getChar
	if (c == 'y')||(c == 'Y') then 
		return True
		else if (c == 'n')||(c == 'N') then
			return False
			else if c /= '\n' then do 
				putStrLn "Caracter invalido, responda con 'y' (yes) o 'n' (no)"
				cPlaying
				else cPlaying


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
	in do
		playerHand h s
		if busted h then do
			putStrLn " Perdiste."
			return (d,h)
			else do 
				putStrLn " ¿Carta o Listo?"
				dCycle d h s j

dCycle :: Hand -> Hand -> String -> String -> IO (Hand,Hand)				
dCycle d h s j = 
	let val = draw d h 
	in do
		c <- getChar
		if (c == 'c')||(c == 'C') then
			drawCycle (fM val) (sM val) s
			else if (c == 'l')||(c == 'L') then do
				putStrLn "Mi turno."
				return (d,h)
				else if c /= '\n' then do 
					putStrLn j
					dCycle d h s j
					else dCycle d h s j

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