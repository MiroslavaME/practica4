--Mora Espinosa Miroslava--echo "# Practica-3" >> README.md
data Arbol a = ArbolBinarioVacio | Raiz a (Arbol a) (Arbol a) deriving Show

-------------------- EJERCICIO 1 --------------------
longitud :: Arbol a -> Int 
longitud ArbolBinarioVacio = 0
longitud (Raiz _ izq der) = 1 + longitud izq + longitud der

-------------------- EJERCICIO 2 --------------------
profundidad :: Arbol a -> Int 
profundidad ArbolBinarioVacio = 0
profundidad (Raiz _ izq der) = 1 + max (profundidad izq) (profundidad der)

-------------------- EJERCICIO 3 --------------------
ancho :: Arbol a -> Int 
ancho ArbolBinarioVacio = 0
ancho (Raiz _ ArbolBinarioVacio ArbolBinarioVacio) = 1
ancho (Raiz _ izq der) = ancho izq + ancho der

-------------------- EJERCICIO 4 --------------------
-------------------- EJERCICIO 4 --------------------
data Recorrido = InOrder | PreOrder | PosOrder

recorrido :: Arbol a -> Recorrido -> [a]
recorrido ArbolBinarioVacio _ = []
recorrido (Raiz x izq der) InOrder  = recorrido izq InOrder ++ [x] ++ recorrido der InOrder
recorrido (Raiz x izq der) PreOrder = [x] ++ recorrido izq PreOrder ++ recorrido der PreOrder
recorrido (Raiz x izq der) PosOrder = recorrido izq PosOrder ++ recorrido der PosOrder ++ [x]


-------------------- EJERCICIO 5 --------------------
niveles :: Arbol a -> [[a]]
niveles arbol = recorrerNiveles [arbol]

recorrerNiveles :: [Arbol a] -> [[a]]
recorrerNiveles [] = []
recorrerNiveles xs = obtenerValores xs : recorrerNiveles (obtenerHijos xs)

obtenerValores :: [Arbol a] -> [a]
obtenerValores [] = []
obtenerValores (ArbolBinarioVacio:resto) = obtenerValores resto
obtenerValores ((Raiz x _ _):resto) = x : obtenerValores resto

obtenerHijos :: [Arbol a] -> [Arbol a]
obtenerHijos [] = []
obtenerHijos (ArbolBinarioVacio:resto) = obtenerHijos resto
obtenerHijos ((Raiz _ izq der):resto) = izq : der : obtenerHijos resto

-------------------- EJERCICIO 6 --------------------
minimo :: (Ord a) => Arbol a -> a
minimo ArbolBinarioVacio = error "Árbol vacío"
minimo (Raiz x ArbolBinarioVacio _) = x
minimo (Raiz _ izq _) = minimo izq

-------------------- EJERCICIO 7 --------------------
maximo :: (Ord a) => Arbol a -> a
maximo ArbolBinarioVacio = error "Árbol vacío"
maximo (Raiz x ArbolBinarioVacio ArbolBinarioVacio) = x
maximo (Raiz x izq der) = maximum [x, maximo izq, maximo der]

-------------------- EJERCICIO 8 --------------------
eliminar :: (Ord a) => Arbol a -> a -> Arbol a
eliminar ArbolBinarioVacio _ = ArbolBinarioVacio
eliminar (Raiz x ArbolBinarioVacio der) y = if y < x
                                              then Raiz x ArbolBinarioVacio der
                                              else if y > x
                                                then Raiz x ArbolBinarioVacio (eliminar der y)
                                                else der

eliminar (Raiz x izq ArbolBinarioVacio) y = if y < x
                                              then Raiz x (eliminar izq y) ArbolBinarioVacio
                                              else if y > x
                                                then Raiz x izq ArbolBinarioVacio
                                                else izq

eliminar (Raiz x izq der) y = if y < x
                                then Raiz x (eliminar izq y) der
                                else if y > x
                                  then Raiz x izq (eliminar der y)
                                  else
                                    let m = minimo der
                                    in Raiz m izq (eliminar der m)
