-- No editar esta parte
main :: IO ()
main = do
  x <- readLn
  print (sumaMenosQueMax (x :: (Int, Int, Int)))

sumaMenosQueMax :: (Int, Int, Int) -> Bool
sumaMenosQueMax (a, b, c) = (maximo a b c > (minimo a b c + medio a b c))

maximo :: Int -> Int -> Int -> Int
maximo a b c  | (a >= b) && (a >= c) = a
              | (b >= c) = b
              | otherwise = c


minimo :: Int -> Int -> Int -> Int
minimo a b c  | (a <= b) && (a <= c) = a
              | (b <= c) = b
              | otherwise = c

medio :: Int -> Int -> Int -> Int
medio a b c   | ((a <= b) && (a >= c)) || ((a <= c) && (a >= b)) = a
              | ((b <= a) && (b >= c)) || ((b <= c) && (b >= a)) = b
              | otherwise = c