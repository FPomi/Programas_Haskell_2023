-- No editar esta parte
main :: IO()
main = do {
  x <- readLn ;
  print(combinacionesMenoresOiguales(x ::(Integer)))
  }

segundaSumatoria :: Integer -> Integer -> Integer -> Integer
segundaSumatoria i j n  | (i * j <= n) && (j == 1) = 1
                        | (j == 1) = 0 
                        | (i * j <= n) = 1 + segundaSumatoria i (j-1) n
                        | otherwise = 0 + segundaSumatoria i (j-1) n 

primeraSumatoria :: Integer -> Integer -> Integer -> Integer
primeraSumatoria i j n  | (i == 1) = segundaSumatoria i j n
                        | otherwise = segundaSumatoria i j n + primeraSumatoria (i-1) j n

combinacionesMenoresOiguales :: Integer -> Integer
combinacionesMenoresOiguales n = primeraSumatoria n n n

