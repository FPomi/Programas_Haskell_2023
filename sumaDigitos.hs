-- No editar esta parte
main :: IO()
main = do {
  x <- readLn ;
  print(sumaDigitos(x ::(Int)))
  }

sumaDigitos :: Int -> Int
sumaDigitos n   | div n 10 < 1 = n
                | otherwise = mod n 10 + sumaDigitos (div n 10)