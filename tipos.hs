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

data RealShape = RealCircle Float Float Float | RealRectangle (Float, Float) (Float, Float) (Float, Float) (Float, Float)

{-
>>> :t RealCircle
RealCircle :: Float -> Float -> Float -> RealShape
>>> :t RealRectangle
RealRectangle :: Float -> Float -> Float -> Float -> RealShape

como se puede apreciar, tanto realcircle como realrectangle són constructuroes
del tipo realshape, otra forma de mirarlo es que son funciones que al final dan 
el tipo realshape
-}

{-
    <!--
        Problema:
            crea una función que dado una RealShape cálcule el area
            y otra función para el cálculo del volumen
    -->
-}

{-
>>> area (RealRectangle (0,2) (1,2) (0,0) (1,0))
2.0
>>> area (RealRectangle (0,2) (2,2) (0,0) (2,0))
4.0
>>> area (RealRectangle (-2,2) (0,2) (-2,0) (0,0))
4.0
>>> area (RealRectangle (-1,2) (1,2) (-1,0) (1,0))
4.0

>>> area (RealCircle 1 1 2)
12.566371
-}

area :: RealShape -> Float
area (RealCircle _ _ r) = pi*r*r
area (RealRectangle tl tr bl _) = abs(fst tr - fst tl) * abs(snd tl - snd bl)
