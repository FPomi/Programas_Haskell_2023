-- Ejercicio 1

fibonacci :: Integer -> Integer
fibonacci n | n == 0 = 0
			| n == 1 = 1
			| otherwise = fibonacci (n-1) + fibonacci (n-2)

------------------------------------------------------------------------------------------------------------------------------------
-- Ejercicio 2

parteEntera :: Float -> Integer
parteEntera n 	| n < 1 = 0
				| otherwise = 1 + parteEntera(n - 1)

------------------------------------------------------------------------------------------------------------------------------------
-- Ejercicio 3

esDivisible :: Integer -> Integer -> Bool
esDivisible n m 	| n == 0 = True
					| n < 0 = False
					| otherwise = esDivisible (n-m) m

------------------------------------------------------------------------------------------------------------------------------------
-- Ejercicio 4

sumaImpares :: Integer -> Integer
sumaImpares n 	| n == 0 = 0
				| otherwise = 1 + 2 * (n-1) + sumaImpares (n-1)

------------------------------------------------------------------------------------------------------------------------------------
-- Ejercicio 5

medioFact :: Integer -> Integer
medioFact n 	| (n == 0 || n == 1) = 1
				| otherwise = n * medioFact (n-2)

------------------------------------------------------------------------------------------------------------------------------------
-- Ejercicio 6


--OPCION 1
--sumaDigitosComparado :: Integer -> Integer -> Integer
--sumaDigitosComparado n m 	| div n m < 1 = 0
--							| m == 10 = mod n m
--							| otherwise = mod n m - mod n (m-10) + sumaDigitosComparado n (m+10) 

--sumaDigitos :: Integer -> Integer
--sumaDigitos n = sumaDigitosComparado n 10


--OPCION 2
sumaDigitos :: Integer -> Integer
sumaDigitos n 	| div n 10 < 1 = n
				| otherwise = mod n 10 + sumaDigitos (div n 10)


------------------------------------------------------------------------------------------------------------------------------------
-- Ejercicio 7

comparadorDeTodosDigitosIguales :: Integer -> Integer -> Bool
comparadorDeTodosDigitosIguales n m 	| n < 1 = True
										| mod n 10 /= m = False
										| otherwise = comparadorDeTodosDigitosIguales (div n 10) m

todosDigitosIguales :: Integer -> Bool 
todosDigitosIguales n = comparadorDeTodosDigitosIguales n (mod n 10)


------------------------------------------------------------------------------------------------------------------------------------
-- Ejercicio 8

--Asumo que m es siempre menor a la cantidad de digitos de n
iesimoDigito :: Integer -> Integer -> Integer
iesimoDigito n m 	| m == 1 = mod n 10
					| otherwise = iesimoDigito (div n 10) (m-1)


------------------------------------------------------------------------------------------------------------------------------------
-- Ejercicio 9

cantidadDeDigitos :: Integer -> Integer
cantidadDeDigitos n 	| div n 10 < 1 = 1
						| otherwise = 1 + cantidadDeDigitos (div n 10)

comparadorCapicua :: Integer -> Integer -> Integer -> Bool
comparadorCapicua n a z 	| (z == 1 && (iesimoDigito n a) == (iesimoDigito n z)) = True
							| (iesimoDigito n a) /= (iesimoDigito n z) = False
							| otherwise = comparadorCapicua n (a+1) (z-1)

esCapicua :: Integer -> Bool
esCapicua n = comparadorCapicua n 1 (cantidadDeDigitos n)


------------------------------------------------------------------------------------------------------------------------------------
-- Ejercicio 10

-- a.
f1 :: Integer -> Integer
f1 n 	| n == 0 = 1 
		| otherwise = (2 ^ n) + f1 (n-1)

--b. 
f2 :: Integer -> Float -> Float
f2 n q 	| n == 0 = 0 
		| otherwise = (q ^ n) + f2 (n-1) q 

--c.
f3 :: Integer -> Float -> Float
f3 n q 	= f2 (2*n) q 

--d. 
f4 :: Integer -> Float -> Float
f4 n q 	= f2 (2*n) q - f2 n q


------------------------------------------------------------------------------------------------------------------------------------
-- Ejercicio 11

--a.
factorial :: Integer -> Integer
factorial n | n == 0 = 1
			| otherwise = n * factorial (n-1)

eAprox :: Integer -> Float
eAprox n 	| n == 0 = 1
			| otherwise = 1 / (fromIntegral (factorial n)) + eAprox (n-1)

--b.
e :: Float 
e = eAprox 10


------------------------------------------------------------------------------------------------------------------------------------
-- Ejercicio 12

aN :: Integer -> Float
aN n 	| n == 1 = 2
		| otherwise = 2 + 1 / aN (n - 1)

raizDe2Aprox :: Integer -> Float
raizDe2Aprox n = aN n - 1


------------------------------------------------------------------------------------------------------------------------------------
-- Ejercicio 13

dobleSumatoria :: Float -> Integer -> Float
dobleSumatoria n m 	| n == 1 = f2 m n
					| otherwise = f2 m n + dobleSumatoria (n-1) m


------------------------------------------------------------------------------------------------------------------------------------
-- Ejercicio 14

sumaPotenciasDesde :: Integer -> Integer -> Integer -> Integer -> Integer
sumaPotenciasDesde q n m i 	| n == 1 && i == 1 = q ^ 2
							| i == 1 = q ^ (n+1) + sumaPotencias q (n-1) m  
							| otherwise = q ^ (n+i) + sumaPotenciasDesde q n m (i-1)

sumaPotencias :: Integer -> Integer -> Integer -> Integer
sumaPotencias q n m = sumaPotenciasDesde q n m m


------------------------------------------------------------------------------------------------------------------------------------
-- Ejercicio 15

fraccionar :: Integer -> Integer -> Float
fraccionar n m 	| n == 0 = 0 
				| otherwise = (fromIntegral n/fromIntegral m) + fraccionar (n-1) m 

sumaRacionales :: Integer -> Integer -> Float
sumaRacionales n m 	| m == 1 = fraccionar n m
					| otherwise = fraccionar n m + sumaRacionales n (m-1)


------------------------------------------------------------------------------------------------------------------------------------
-- Ejercicio 16

--a.
menorDivisorDesde :: Integer -> Integer -> Integer
menorDivisorDesde n i 	| mod n i == 0 = i
						| otherwise = menorDivisorDesde n (i+1)

menorDivisor :: Integer -> Integer
menorDivisor n = menorDivisorDesde n 2

--b. 
esPrimo :: Integer -> Bool
esPrimo n 	| n == 1 = False
			| menorDivisor n == n = True
			| otherwise = False

--c.
sonCoprimosDesde :: Integer -> Integer -> Integer -> Bool
sonCoprimosDesde n m i 	| n == 1 || m == 1 || i == 1 = True
						| mod n i == 0 && mod m i == 0 = False
						| otherwise = sonCoprimosDesde n m (i-1)
						

sonCoprimos :: Integer -> Integer -> Bool
sonCoprimos n m = sonCoprimosDesde n m m

--d.
nEsimoPrimoDesde :: Integer -> Integer -> Integer
nEsimoPrimoDesde n i 	| n == 0 = i 
						| esPrimo i == True && n /= 1 = nEsimoPrimoDesde (n-1) (i+1)
						| esPrimo i == True && n == 1 = nEsimoPrimoDesde (n-1) i
						| otherwise = nEsimoPrimoDesde n (i+1)

nEsimoPrimo :: Integer -> Integer
nEsimoPrimo n = nEsimoPrimoDesde n 2


------------------------------------------------------------------------------------------------------------------------------------
-- Ejercicio 17

esFibonacciDesde :: Integer -> Integer -> Bool
esFibonacciDesde n i 	| n == fibonacci i = True
						| n < fibonacci i = False
						| otherwise = esFibonacciDesde n (i+1)

esFibonacci :: Integer -> Bool
esFibonacci n = esFibonacciDesde n 0


------------------------------------------------------------------------------------------------------------------------------------
-- Ejercicio 18

mayorDigitoParDesdeConMayor :: Integer -> Integer -> Integer -> Integer
mayorDigitoParDesdeConMayor n i x 	| i > cantidadDeDigitos n = x
								 	| mod (iesimoDigito n i) 2 == 0 && (iesimoDigito n i) > x = mayorDigitoParDesdeConMayor n (i+1) (iesimoDigito n i)  
								 	| otherwise = mayorDigitoParDesdeConMayor n (i+1) x  

mayorDigitoPar :: Integer -> Integer
mayorDigitoPar n = mayorDigitoParDesdeConMayor n 1 (-1)


------------------------------------------------------------------------------------------------------------------------------------
-- Ejercicio 19

sumatoriaDeNPrimosDesde :: Integer -> Integer -> Integer
sumatoriaDeNPrimosDesde n i | i == n = nEsimoPrimo i
							| otherwise = nEsimoPrimo i + sumatoriaDeNPrimosDesde n (i+1) 

sumatoriaDeNPrimos :: Integer -> Integer
sumatoriaDeNPrimos n = sumatoriaDeNPrimosDesde n 1

esSumaInicialDePrimosDesde :: Integer -> Integer -> Bool
esSumaInicialDePrimosDesde n m 	| n == sumatoriaDeNPrimos m = True
								| n < sumatoriaDeNPrimos m = False
								| otherwise = esSumaInicialDePrimosDesde n (m+1)

esSumaInicialDePrimos :: Integer -> Bool
esSumaInicialDePrimos n = esSumaInicialDePrimosDesde n 1


------------------------------------------------------------------------------------------------------------------------------------
-- Ejercicio 20

sumaDivisoresDesde :: Integer -> Integer -> Integer 
sumaDivisoresDesde n i 	| i == n = n
						| mod n i == 0 = i + sumaDivisoresDesde n (i+1)
						| otherwise = sumaDivisoresDesde n (i+1)

sumaDivisores :: Integer -> Integer
sumaDivisores n = sumaDivisoresDesde n 1 

tomaValorMaxConMax :: Integer -> Integer -> Integer -> Integer
tomaValorMaxConMax n1 n2 x 	| n1 > n2 = x 
							| sumaDivisores n1 >= sumaDivisores x = tomaValorMaxConMax (n1+1) n2 n1
							| otherwise = tomaValorMaxConMax (n1+1) n2 x

tomaValorMax :: Integer -> Integer -> Integer
tomaValorMax n1 n2 = tomaValorMaxConMax n1 n2 n1


------------------------------------------------------------------------------------------------------------------------------------
-- Ejercicio 21 



pitagoras :: Integer -> Integer -> Integer -> Integer
pitagoras m n r = pitagorasDesde m n r n

pitagorasDesde :: Integer -> Integer -> Integer -> Integer -> Integer
pitagorasDesde m n r i 	| (m == 0) && (i == 0) && (m^2 + i^2 <= r^2) = 1
						| (m == 0) && (i == 0) = 0
						| i == 0 && (m^2 + i^2 <= r^2) = 1 + pitagorasDesde (m-1) n r n 
						| i == 0 = pitagorasDesde (m-1) n r n 
						| (m^2 + i^2 <= r^2) = 1 + pitagorasDesde m n r (i-1)
						| otherwise = pitagorasDesde m n r (i-1)