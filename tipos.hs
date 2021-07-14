{-
    para crear un tipo en haskell
    se usa la palabra `data`. Un tipo puede tener varios
    constructores de tipo

>>> :t True
True :: Bool
>>> :t False
False :: Bool

El tipo Bool se define como;
    data Bool = True | False
-}

{-
    vamos a definir el tipo figura; las figuras soportadas de momento
    son el circulo y el rectangulo
-}
data Shape = Circle | Rectangle
{-
>>> :t Circle
Circle :: Shape
>>> :t Rectangle
Rectangle :: Shape

ahora bien, definir un tipo de esta manera es inutil,
necesitamos poder crear las figuras con valores iniciales
-}

{-
    new type te permite tener un constructor de tipo sobre un tipo ya existente
    en tiempo de ejecucion sigue siendo el tipo (Float, Float) però en compilaciñon
    el compilador vigila no confundirlo con una tupla de floats
-}
newtype Point2d = Point2d (Float, Float) deriving (Show)

data Shape2d = 
        Circle2d Point2d Float 
    |   Rectangle2d Point2d Point2d Point2d Point2d
    deriving (Show) -- de esta forma se puede representar el tipo como cadena

{-
>>> :t Circle2d
Circle2d :: Float -> Float -> Float -> Shape2d
>>> :t Rectangle2d
Rectangle2d :: Float -> Float -> Float -> Float -> Shape2d

como se puede apreciar, tanto realcircle como Rectangle2d són constructuroes
del tipo realshape, otra forma de mirarlo es que son funciones que al final dan 
el tipo realshape
-}

{-
    <!--
        Problema:
            crea una función que dado una Shape2d cálcule el area
            y otra función para el cálculo del volumen
    -->
-}

{-
>>> area (Rectangle2d (0,2) (1,2) (0,0) (1,0))
2.0
>>> area (Rectangle2d (0,2) (2,2) (0,0) (2,0))
4.0
>>> area (Rectangle2d (-2,2) (0,2) (-2,0) (0,0))
4.0
>>> area (Rectangle2d (-1,2) (1,2) (-1,0) (1,0))
4.0

>>> area (Circle2d 1 1 2)
12.566371
-}

area :: Shape2d -> Float
area (Circle2d _ r) = pi*r*r
area (Rectangle2d (Point2d tl) (Point2d tr) (Point2d bl) _) = abs(fst tr - fst tl) * abs(snd tl - snd bl)
