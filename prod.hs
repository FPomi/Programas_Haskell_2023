-- No editar esta parte
main :: IO()
main = do {
  x <- readLn ;
  print(prod(x ::(Integer)))
  }

productoria :: Integer -> Integer
productoria i   | (i == 1) = 3
                | otherwise = ((i ^ 2) + 2 * i) * productoria (i-1)

prod :: Integer -> Integer
prod n  = productoria (2*n)