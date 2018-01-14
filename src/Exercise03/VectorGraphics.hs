{-# LANGUAGE OverloadedStrings #-}

module Exercise03.VectorGraphics where

import Exercise03.Vectors
import Graphics.Svg
import Data.Text

-- Data types

data Picture = Last Element' | More Element' Picture

data Element' = 
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

triangleCenter (Triangle (Vector x1 y1) (Vector x2 y2) (Vector x3 y3)) =
    Vector ((x1+x2+x3)/3) ((y1+y2+y3)/3)

-- Draw using the SVG library

svgize (Line (Vector x1 y1) (Vector x2 y2)) = ([
        D_ <<-
        mA x1 y1
        <> (lA x2 y2)
    ], path_)
svgize (Rectangle start@(Vector x y) width height) = ([
        D_ <<-
        mA x y
        <> (lA x (y + height))
        <> (lA (x + width) (y + height))
        <> (lA (x + width) y)
    ], path_)
svgize (Triangle (Vector x1 y1) (Vector x2 y2) (Vector x3 y3)) = ([
        D_ <<-
        mA x1 y1
        <> (lA x2 y2)
        <> (lA x3 y3)
        <> (lA x1 y1)
    ], path_)
svgize (Circle (Vector x y) radius) = ([
        Cx_ <<- pack (show x),
        Cy_ <<- pack (show y),
        R_ <<- pack (show radius)
    ], circle_)

drawElement :: Term a => Element' -> a
drawElement element = f (elem ++ [
        Fill_opacity_ <<- "0",
        Stroke_ <<- pack "black"
    ])
    where (elem, f) = svgize element

drawPicture :: Picture -> Element
drawPicture (Last element) = drawElement element
drawPicture (More element rest) = (drawElement element) <> (drawPicture rest)

-- TODO Transformations

-- move :: (Double, Double) -> Picture -> Picture
-- scale :: Double -> Picture -> Picture
-- rotate :: (Double, Double) -> Double -> Picture -> Picture

-- Examples

triangleCube :: Double -> Picture
triangleCube sideLen =
    More (Rectangle (Vector 0 0) sideLen sideLen) $
    Last (
        Triangle (Vector 0 sideLen)
        (Vector sideLen sideLen)
        (Vector (sideLen/2) 0)
        )

house :: Double -> Picture
house sideLen =
    More t $
    More (Circle (triangleCenter t) (sideLen/4)) $
    Last (Rectangle (Vector 0 sideLen) sideLen sideLen)
    where t = Triangle (Vector (sideLen/2) 0) (Vector 0 sideLen) (Vector sideLen sideLen)

myLine = Line (Vector 0 0) (Vector 100 100)
myRektangle = Rectangle (Vector 0 0) 20 20
myTriangle = Triangle (Vector 10 10) (Vector 0 20) (Vector 60 20)
myCircle = Circle (Vector 50 50) 10

myPicture = More myLine $
            More myRektangle $
            More myTriangle $
            Last myCircle

-- TODO dragon :: Int -> Picture

-- Output

svg = doctype <> with (svg11_ (drawPicture (house 50))) [Width_ <<- "100", Height_ <<- "100"]
mainSVG = renderToFile "output.svg" svg
