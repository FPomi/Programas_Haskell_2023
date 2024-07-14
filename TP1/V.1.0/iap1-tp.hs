--module Solucion (nombresDeUsuarios, amigosDe, cantidadDeAmigos, usuarioConMasAmigos, estaRobertoCarlos,
--                publicacionesDe, publicacionesQueLeGustanA, lesGustanLasMismasPublicaciones, tieneUnSeguidorFiel,
--                existeSecuenciaDeAmigos) where

-- Completar con los datos del grupo
--
-- Nombre de Grupo: xx
-- Integrante 1: Nombre Apellido, email, LU
-- Integrante 2: Nombre Apellido, email, LU
-- Integrante 3: Nombre Apellido, email, LU
-- Integrante 4: Nombre Apellido, email, LU

------------------------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------------------------

type Usuario = (Integer, String) -- (id, nombre)
type Relacion = (Usuario, Usuario) -- usuarios que se relacionan
type Publicacion = (Usuario, String, [Usuario]) -- (usuario que publica, texto publicacion, likes)
type RedSocial = ([Usuario], [Relacion], [Publicacion])

------------------------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Funciones basicas

usuarios :: RedSocial -> [Usuario]
usuarios (us, _, _) = us

relaciones :: RedSocial -> [Relacion]
relaciones (_, rs, _) = rs

publicaciones :: RedSocial -> [Publicacion]
publicaciones (_, _, ps) = ps

idDeUsuario :: Usuario -> Integer
idDeUsuario (id, _) = id 

nombreDeUsuario :: Usuario -> String
nombreDeUsuario (_, nombre) = nombre 

primerUsuarioDeRelacion :: Relacion -> Usuario
primerUsuarioDeRelacion (u1, _) = u1

segundoUsuarioDeRelacion :: Relacion -> Usuario
segundoUsuarioDeRelacion (_, u2) = u2

usuarioDePublicacion :: Publicacion -> Usuario
usuarioDePublicacion (u, _, _) = u

textoDePublicacion :: Publicacion -> String
textoDePublicacion (_, t, _) = t

likesDePublicacion :: Publicacion -> [Usuario]
likesDePublicacion (_, _, us) = us

------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Ejercicios
------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Auxiliares

--Comprueba si un elemento se encuentra repetido en un conjunto 
pertenece :: (Eq elemento) => elemento -> [elemento] -> Bool
pertenece elemento elementos    | elementos == [] = False
                                | head (elementos) == elemento = True
                                | otherwise = pertenece elemento (tail(elementos))

--Dado un conjunto (ts), devuelve el mismo conjunto pero sin sus valores repetidos
sinRepetidos :: (Eq elemento) => [elemento] -> [elemento]
sinRepetidos elementos  | elementos == [] = []
                        | pertenece (head elementos) (tail elementos) = sinRepetidos (tail(elementos))
                        | otherwise = [head (elementos)] ++ sinRepetidos (tail(elementos))

--Cuenta la cantidad de elementos en un conjunto
contadorDeElementos :: (Eq elemento) => [elemento] -> Int
contadorDeElementos elementos   | elementos == [] = 0
                                | otherwise = 1 + contadorDeElementos (tail(elementos))

--Compara los elementos de dos conjuntos para ver si son iguales
mismosElementos :: (Eq elemento) => [elemento] -> [elemento] -> Bool
mismosElementos elementos1 elementos2   | elementos1 == [] = True
                                        | pertenece (head elementos1) elementos2 = mismosElementos (tail elementos1) elementos2 
                                        | otherwise = False

------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Ejercicio 1 

--Indica todos los nombres de todos los usuarios registrados en una red social
nombresDeUsuariosConRepetidos :: RedSocial -> [String]
nombresDeUsuariosConRepetidos (us, rs, ps)  | us == [] = []
                                            | otherwise  = [nombreDeUsuario(head(us))] ++ nombresDeUsuarios (tail(us), rs, ps)
 

--Indica los nombres de los usuarios registrados dentro de una red social (sin sus valores repetidos)
nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios r = sinRepetidos (nombresDeUsuariosConRepetidos r)
 

------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Ejercicio 2

-- Devuelve un conjunto de usuarios que tienen una relacion con el usuario indicado
amigosRepetidosDe :: RedSocial -> Usuario -> [Usuario]
amigosRepetidosDe (us, rs, ps) u    | rs == [] = []
                                    | u == primerUsuarioDeRelacion (head(rs)) = [segundoUsuarioDeRelacion(head(rs))] ++ amigosRepetidosDe (us, tail(rs), ps) u
                                    | u == segundoUsuarioDeRelacion(head(rs)) = [primerUsuarioDeRelacion(head(rs))] ++ amigosRepetidosDe (us, tail(rs), ps) u
                                    | otherwise = amigosRepetidosDe (us, tail(rs), ps) u

-- Devuelve un conjunto de usuarios que tienen una relacion con el usuario indicado (sin sus valores repetidos)
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe r u = sinRepetidos (amigosRepetidosDe r u)

------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Ejercicio 3

-- Devuelve la cantidad de amigos que tiene un usuario 
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos r u = contadorDeElementos (amigosDe r u) 

------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Ejercicio 4

-- Compara si un ususario tiene mas amigos que el resto de usuarios restantes en la red
tieneMasAmigosQue :: RedSocial -> Usuario -> [Usuario] -> Bool
tieneMasAmigosQue r u us    | us == [] = True
                            | cantidadDeAmigos r u < cantidadDeAmigos r (head us) = False
                            | otherwise = tieneMasAmigosQue r u (tail us) 

-- Devuelve al ususario con mas amigos
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos (us, rs, ps)    | tieneMasAmigosQue (us, rs, ps) (head(us)) (tail(us)) = head (us)
                                    | otherwise = usuarioConMasAmigos (tail(us), rs, ps)

------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Ejercicio 5

-- Determina si existe algun usuario en una red social con mas de 1000000 de amigos
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos r | cantidadDeAmigos r (usuarioConMasAmigos r) > 1000000 = True
                    | otherwise = False

------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Ejercicio 6

-- Devuelve un conjunto de todas publicaciones del usuario indicado
todasLasPublicacionesDe :: RedSocial -> Usuario -> [Publicacion]
todasLasPublicacionesDe (us, rs, ps) u  | ps == [] = []
                                        | usuarioDePublicacion(head(ps)) == u = [head(ps)] ++ todasLasPublicacionesDe (us, rs, tail(ps)) u
                                        | otherwise = todasLasPublicacionesDe (us, rs, tail(ps)) u

-- Devuelve un conjunto de publicaciones del usuario indicado (sin sus valores repetidos)
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe r u = sinRepetidos (todasLasPublicacionesDe r u)

------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Ejercicio 7

-- Devuelve un conjunto de todas las publicaciones que le gustan al usuario indicado
todasLasPublicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
todasLasPublicacionesQueLeGustanA (us, rs, ps) u    | ps == [] = []
                                                    | pertenece u (likesDePublicacion(head ps)) = [head(ps)] ++ todasLasPublicacionesQueLeGustanA (us, rs, tail(ps)) u
                                                    | otherwise = todasLasPublicacionesQueLeGustanA (us, rs, tail(ps)) u

-- Devuelve un conjunto de publicaciones que le gustan al usuario indicado (sin sus valores repetidos)
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA r u = sinRepetidos (todasLasPublicacionesQueLeGustanA r u)

------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Ejercicio 8

-- Comprueba si a dos usuarios les gustan las mismas publicaciones
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones r u1 u2 = mismosElementos (publicacionesQueLeGustanA r u1) (publicacionesQueLeGustanA r u2)

------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Ejercicio 9

-- Comprueba si es que existe algun usuario de la red al cual le gusten todas las publicaiones del ususario determinado (sin incluirlo)
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel (us, rs, ps) u  | (publicacionesDe (us, rs, ps) u == [] || us == []) = False
                                    | ((head(us) /= u) && (mismosElementos (publicacionesQueLeGustanA (us, rs, ps) (head us)) (publicacionesDe (us, rs, ps) u))) = True
                                    | otherwise = tieneUnSeguidorFiel (tail us, rs, ps) u

------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Ejercicio 10

-- Devuelve true si el primer ususario ingresado es amigo del siguiente en la lista (usuario 1 amigo de usuario 2), y asi hasta llegar al segundo usuario ingresado
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos (us, rs, ps) primero ultimo | contadorDeElementos us < 2 = False 
                                                    | actual /= primero = existeSecuenciaDeAmigos (tail us, rs, ps) primero ultimo
                                                    | siguiente == ultimo && pertenece actual (amigosDe (us, rs, ps) ultimo) = True
                                                    | pertenece actual (amigosDe (us, rs, ps) siguiente) = existeSecuenciaDeAmigos (tail us, rs, ps) siguiente ultimo
                                                    | otherwise = False
                                                    where
                                                        actual = head us
                                                        siguiente = head(tail us)

------------------------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------------------------
