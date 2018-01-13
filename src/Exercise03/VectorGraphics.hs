module Exercise03.VectorGraphics where

import Exercise03.Vectors

data Picture =
    Line {
        lineStart :: Vector Double,
        lineEnd :: Vector Double
    } |
    Rectangle {
        rectStart :: Vector Double,
        rectWidth :: Double,
        rectHeight :: Double
    } |
    Circle {
        circleCenter :: Vector Double,
        circleRadius :: Double
    } |
    Triangle {
        triPtA :: Vector Double,
        triPtB :: Vector Double,
        triPtC :: Vector Double
    }
    deriving (Show, Eq)

triangleCube :: Double -> [Picture]
triangleCube sideLen = [
        Rectangle (Vector 0 0) sideLen sideLen,
        Triangle (Vector 0 sideLen)
                (Vector sideLen sideLen)
                (Vector (sideLen/2) 0)
    ]

-- TODO draw using the SVG library