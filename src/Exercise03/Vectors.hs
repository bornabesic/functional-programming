module Exercise03.Vectors where

{--
data Vector2D = Vector2D {
                    x :: Double,
                    y :: Double
                }

instance Num Vector2D where
    (Vector2D x1 y1) + (Vector2D x2 y2) = Vector2D (x1 + x2) (y1 + y2)
    negate (Vector2D x y) = Vector2D (- x) (- y)

instance Show Vector2D where
    show (Vector2D x y) = show (x, y)
--}

data Vector t = Vector {
                    x :: t,
                    y :: t
                }
                deriving (Eq)

instance Show t => Show (Vector t) where
    show (Vector x y) = show (x, y)

instance Num t => Num (Vector t) where
    (Vector x1 y1) + (Vector x2 y2) = Vector (x1 + x2) (y1 + y2)
    (Vector x1 y1) * (Vector x2 y2) = Vector (x1 * x2 + y1 * y2) 0
    negate (Vector x y) = Vector (- x) (- y)
    signum (Vector x y) = Vector (signum x) (signum y)
    abs (Vector x y) = Vector (abs x) (abs y)
    fromInteger i = undefined
