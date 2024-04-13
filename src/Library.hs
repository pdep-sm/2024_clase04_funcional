module Library where
import PdePreludat



data Pedido = Pedido {
    usuario :: String,
    fechaEntrega :: Fecha,
    direccion :: String,
    productos :: [Producto],
    estado :: String
} deriving Show

data Producto = Producto {
    nombre :: String,
    precio :: Number,
    cantidad :: Number
} deriving Show

data Fecha = Fecha {
    dia :: String,
    mes :: String,
    anio :: String
} deriving Show

-- esProductoCaro producto = ( (>1000) . precio ) producto
esProductoCaro = (>1000) . precio

productosCaros pedido = (productosCarosRec . productos) pedido

productosCarosRec [] = []
productosCarosRec (p:ps)
    | esProductoCaro p = p : productosCarosRec ps
    | otherwise = productosCarosRec ps

-- que pasa si quiero los productos que empiezan con A
productosConA [] = []
productosConA (p:ps)
    | esProductoConA p = p : productosConA ps
    | otherwise = productosConA ps

esProductoConA p = ( (=='a') . head . nombre ) p

-- que pasa si quiero los productos con cantidad mayor a N
productosConCantidadMayorA _ [] = []
productosConCantidadMayorA n (p:ps)
    | ( (>n). cantidad ) p = p : productosConCantidadMayorA n ps
    | otherwise = productosConCantidadMayorA n ps
{-
Vemos que se puede deducir una lógica común a todas
filter' _ [] = []
filter' f (x:xs)
    | f x = x : filter' f xs
    | otherwise = filter' f xs

Donde Filter es de orden superior, porque recibe una función de parámetro
Y no necesitamos definirla, la podemos usar directamente!
-}
-- ya no necesitamos preocuparnos por recorrer la lista, y podriamos redefinir productosCaros
productosCaros' pedido = (filter esProductoCaro . productos) pedido

-- Bienvenidos al ORDEN SUPERIOR: funciones que reciben funciones
-- podríamos ahora también redefinir productosConA
productosConA' ps = filter ( (=='a') . head . nombre ) ps

-- puedo también redefinir productosConCantidadMayorA
productosConCantidadMayorA' n ps = filter ( (>n) . cantidad ) ps

-- Amiga de filter, es map que nos permite aplicar una transformación a todos los elementos de la lista
{-
si hicieramos su definición
map _ [] = []
map f (x:xs) = f x : map f xs
-}
-- Por ejemplo si quiero los precios de una lista de productos...
preciosProductos ps = map precio ps



