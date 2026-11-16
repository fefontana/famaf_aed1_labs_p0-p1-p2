--obs el problema en esta definicion de funcion
--realmente no sabe como comparar que dos elementos sean iguales
--porque comparar numeros si sabe
--bastaria con la siguiente firma o signature
--pert :: Int -> [Int] -> Bool
--la solucion es

--Haskell es un lenguaje tipado.
--su sistema de tipos es estatico de modo que el tipo de cualquier expresion
--se conoce en tiempo de compilacion resultando en codigo mas seguro.
--
--Ej un programa que intente hacer 2/True captura este error en tiempo de 
--compilacion en lugar de que el programa falle en tiempo de ejecucion
--
--El hecho de que en Haskell todo tiene un tipo le permite al compilador
--razonar sobre el programa antes de compilarlo.
--
--Ademas Haskell posee inferencia de tipos en vez de tipado manifiesto o
--anotacion explisita de tipos donde se declara manualmente el tipo de cada
--variable, parametro o funcion en el codigo. 
--Ej en C: int x = 5
--En haskell es el compilador quien trabaja en la inferencia ya que deduce 
--automáticamente los tipos analizando cómo se usan los valores y las 
--funciones en el código.
--Por ejemplo escribir lo siguiente sin declarar su tipo: 
--doble x = x * 2
--GHC deduce por sí solo que su firma es Num a => a -> a.
--
--Aunque Haskell tiene inferencia completa, escribir las firmas de tipo 
--(como esCero :: Int -> Bool) se considera una buena práctica para documentar 
--el código y ayudar al compilador a dar mejores mensajes de error.
--
--En Haskell una clase de tipos es una especie de interfaz que define 
--un comportamiento. No son como las clases de los lenguajes OO.
--Un tipo en Haskell puede ser una instancia de esa clase de tipos si soporta 
--ese comportamiento. 
--Ejemplo: El tipo Int es una instancia de la clase Eq, ya que la clase de 
--tipos Eq define el comportamiento de cosas que se pueden equiparar. 
--
--No es posible hacer una función polimórfica (para cualquier tipo a) que compruebe pertenencia en una lista sin la restricción Eq a o sin pasarle una función de comparación.
--Para saber si un elemento está adentro de una lista, Haskell necesita algún mecanismo para comparar si dos valores son iguales. Sin embargo, tenés dos alternativas para escribir la firma sin poner Eq a =>:
--1. Usar un tipo concreto (por ejemplo, Int o Char)
--Si tu función trabaja solo con un tipo específico que ya sabe compararse por igualdad, no necesitás escribir Eq en la firma:
--2. Pasar la función de igualdad como parámetro (Orden superior)
--Si querés que funcione para cualquier tipo a (incluso tipos que no tienen Eq, como funciones) sin usar la clase de tipos Eq, podés recibir como primer argumento una función (a -> a -> Bool) que diga cómo comparar dos elementos:
--
--pert :: a -> [a] -> Bool
pert :: (Eq a) => a -> [a] -> Bool
pert _ [] = False
pert e (x:xs)
  | e == x    = True
  | otherwise = pert e xs


--esCero verifica si un entero es igual a 0.
esCero :: Int -> Bool
esCero n
 | n==0 = True
 | otherwise = False


--esPositivo verifica si un entero es estrictamente mayor a 0.
esPositivo :: Int -> Bool
esPositivo n
 | n>0 = True
 | otherwise = False


--esPositivo verifica si un entero es estrictamente mayor a 0 sin condicional.
esPositivo' :: Int -> Bool
esPositivo' n = n>0


--Ejecutar estas funciones con numeros negativos da error. ¿Por que?

--No instance for ‘Num (Int -> Bool)’ arising from a use of ‘-’
--(maybe you haven't applied a function to enough arguments?)
--In the expression: esPositivo' - 1
--In an equation for ‘it’: it = esPositivo' - 1


-- ¿Por qué ocurre este error?
--Por la precedencia del signo menos ocurre un fallo de tipos.
--Como no se puede hacer una resta aritmética con una función (Int -> Bool), 
--GHC arroja el error No instance for 'Num (Int -> Bool)'.

--En Haskell, cuando escribís esPositivo -1, el compilador no lo lee 
--como "aplicar esPositivo al número -1", sino que intenta restarle el 
--número 1 a la función esPositivo (esPositivo - 1).

--Solucion:

--ghci> esPositivo' (-1)
--False
--ghci> esCero (-2)
--False

--los conjuntos se pueden definir por extensión o por comprensión
--a si mismo las listas extensiva o intensivamente

--esVocal verifica si un caracter es una vocal en minuscula.
esLetra :: Char -> Bool
esLetra c = pert c ['a','b','c','x','y','z']

-- definicion de conjuntos lista por comprension

esVocal :: Char -> Bool
esVocal c = pert c "aeiou"
