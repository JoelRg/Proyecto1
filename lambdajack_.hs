import System.Random
import Cards
import LambdaJack

--- BIENVENIDO A LAMBDAJACK -----------------------------------------
--- Joel Rivas 11-10866 ---------------------------------------------
--- Larry Perez 10-10547 --------------------------------------------
	
main :: IO ()
main = do
	s <- welcome
	gen <- newStdGen
	gameloop (GS 0 0 s gen)
