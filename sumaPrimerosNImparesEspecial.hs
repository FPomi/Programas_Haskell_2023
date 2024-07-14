-- No editar esta parte
main :: IO()
main = do {
  x <- readLn ;
  print(sumaPrimerosNImparesEspecial(x ::(Integer)))
  }

sumaPrimerosNImpares :: Integer -> Integer
sumaPrimerosNImpares i    | i == 1 = 4
                          | mod i 2 == 0 = 0 + sumaPrimerosNImpares (i-1)
                          | otherwise = ((2*i) + 2) + sumaPrimerosNImpares (i-1)

sumaPrimerosNImparesEspecial :: Integer -> Integer
sumaPrimerosNImparesEspecial n = sumaPrimerosNImpares ((2 * n) - 1)


