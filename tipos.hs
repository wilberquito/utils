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

{-
    <!-- 
        problema:
            crea una función que desplace una figura sobre el eje x
    -->

>>> let p = Point2d (1,1)
>>> p
Point2d (1.0,1.0)
>>> let c = Circle2d p 2
>>> c
Circle2d (Point2d (1.0,1.0)) 2.0

>>> let ct = transformX c 2
>>> ct
Circle2d (Point2d (3.0,1.0)) 2.0

>>> let lt = Point2d (-1,1)
>>> lt
Point2d (-1.0,1.0)


>>> let rt = Point2d (1,1)
>>> rt
Point2d (1.0,1.0)

>>> let lb = Point2d (-1,-1)
>>> lb
Point2d (-1.0,-1.0)


>>> let rb = Point2d (1,-1)
>>> rb
Point2d (1.0,-1.0)

>>> let rec = Rectangle2d lt rt lb rb
>>> rec
Rectangle2d (Point2d (-1.0,1.0)) (Point2d (1.0,1.0)) (Point2d (-1.0,-1.0)) (Point2d (1.0,-1.0))
>>> let rect = transformX rec 2
>>> rect
Rectangle2d (Point2d (1.0,1.0)) (Point2d (3.0,1.0)) (Point2d (1.0,-1.0)) (Point2d (3.0,-1.0))
-}

transformX :: Shape2d -> Float -> Shape2d
transformX (Circle2d (Point2d c) r) x = Circle2d (Point2d (fst c + x, snd c)) r
transformX (Rectangle2d (Point2d tl) (Point2d tr) (Point2d bl) (Point2d br)) x = 
    Rectangle2d (Point2d (fst tl + x, snd tl)) (Point2d (fst tr + x, snd tr)) (Point2d (fst bl + x, snd bl)) (Point2d (fst br + x, snd br))


curriculum :: [(String, Float)]
curriculum = [
    ("algebra", 6),
    ("calcul", 6.5),
    ("logica i mat discreta", 7),
    ("mtp1", 5.6),
    ("etc1", 6.1),
    ("fisica i electronica", 7),
    ("mtp2", 6.5),
    ("etc2", 7.7),
    ("estadistica", 6.3),
    ("organizacion i adm empresa", 5.8),
    ("eda", 5.8),
    ("projecte de programacio", 6.5),
    ("sistemes digitals", 7.3),
    ("computadors", 7.2),
    ("bd", 6.3),
    ("engenyieria soft 1", 6.1),
    ("sistemes operatious", 6.4),
    ("multimedia", 8),
    ("fonaments de computacio", 7.3),
    ("paradigmes", 7),
    ("inteligencia artificial", 6.4),
    ("arquitectura computadors", 6),
    ("robotica", 7.3),
    ("engenyeria soft 2", 5.5),
    ("projecte desenvolupament soft", 7.8),
    ("projecte sistemes operatius", 7.6),
    ("xarxes", 6.7),
    ("projecte de xarxes", 7.4),
    ("legislacio", 6.5),
    ("estades en entorn laboral", 9),
    ("informatica grafica", 8.1),
    ("criptografica", 7.8),
    ("computacio numerica", 6)]

todo :: [String]
todo = [
    "tecnicas avanzadas IA",
    "programacio declartiva",
    "compiladores",
    "TFG"]

avarage :: [Float] -> Float
avarage ls =   sum ls / fromIntegral (length ls)  