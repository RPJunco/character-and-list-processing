-- Trabajo Practico N°2

-- Punto 1.a

funcion1::Char -> Bool
funcion1 car
	| 'a' == car || 'e' == car || 'i' == car || 'o' == car || 'u' == car = True
	| 'A' == car || 'E' == car || 'I' == car || 'O' == car || 'U' == car = True
	|otherwise = False
	
-- Punto 1.b

funcion2::Char -> Bool
funcion2 car = if '0' == car || '1' == car || '2' == car || '3' == car || '4' == car || 
	'5' == car || '6' == car || '7' == car || '8' == car || '9' == car then True else False
	

-- Punto 2

funcion3:: Integer -> Char -> Integer
funcion3 x car
	| funcion1 car = 2 * x
	| funcion2 car = 3 * x
	| otherwise = x


-- Punto 3

funcion4::[Integer] -> Char -> [Integer]
funcion4 [] _ = []
funcion4 (x:xs) car
	| funcion1 car = [x * 2] ++ funcion4 xs car
	| funcion2 car = [x * 3] ++ funcion4 xs car
	| otherwise = (x:xs)
	
	
-- Punto 4

-- Funcion auxiliar para contar la cantidad de elementos de la lista
contarElem::[Integer] -> Integer
contarElem [] = 0
contarElem (x:xs) = 1 + contarElem xs

funcion5::[Integer] -> Char -> Int -> [Integer]
funcion5 [] _ _ = []
funcion5 (x:xs) car cant
	| funcion1 car && cant <= cantLista = take cant ([x * 2] ++ funcion5 xs car cant)
	| funcion1 car && cant > cantLista = take cantLista ([x * 2] ++ funcion5 xs car cant)
	| funcion2 car && cant <= cantLista = take cant ([x * 3] ++ funcion5 xs car cant)
	| funcion2 car && cant > cantLista = take cantLista ([x * 3] ++ funcion5 xs car cant)
	| cant <= cantLista = take cant ([x] ++ funcion5 xs car cant)
	| otherwise = (x:xs)
	where cantLista = fromIntegral(contarElem (x:xs))


-- Punto 5

funcion6::Integer -> (Integer,Integer)
funcion6 x = (x,x*3)


-- Punto 6

funcion7:: Integer -> Integer -> [(Integer, Integer)]
funcion7 n cant 
    | cant <= 0 = []
    | otherwise = funcion6 n : funcion7 (3 * n) (cant - 1)

