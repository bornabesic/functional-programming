{-# LANGUAGE OverloadedStrings #-}

module Exercise03.VectorGraphics where

import Exercise03.Vectors
import Graphics.Svg
import Data.Text

-- Data types

data Picture = Picture [Element']

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

{--drawPicture :: Picture -> Element
drawPicture (Last element) = drawElement element
drawPicture (More element rest) = (drawElement element) <> (drawPicture rest)
--}

drawPicture :: Picture -> Element
drawPicture (Picture (e:es)) = Prelude.foldr foldFunc (drawElement e) es
    where foldFunc element acc = (drawElement element) <> acc

-- Transformations

-- Move
moveElement :: (Double, Double) -> Element' -> Element'
moveElement (dx, dy) element = case element of
    (Line start end) -> Line (start + translateVector) (end + translateVector)
    (Rectangle start width height) -> Rectangle (start + translateVector) width height
    (Circle center radius) -> Circle (center + translateVector) radius
    (Triangle a b c) -> Triangle (a + translateVector) (b + translateVector) (c + translateVector)
    where translateVector = Vector dx dy

move :: (Double, Double) -> Picture -> Picture
move d (Picture es) = Picture $ Prelude.map (moveElement d) es

-- Scale
scaleElement :: Double -> Element' -> Element'
scaleElement s element = case element of
    (Line (Vector xs ys) (Vector xe ye)) -> Line (Vector (s*xs) (s*ys)) (Vector (s*xe) (s*ye))
    (Rectangle (Vector xs ys) width height) -> Rectangle (Vector (s*xs) (s*ys)) (s*width) (s*height)
    (Circle (Vector xc yc) radius) -> Circle (Vector (s*xc) (s*yc)) (radius*s)
    (Triangle (Vector x1 y1) (Vector x2 y2) (Vector x3 y3)) -> Triangle (Vector (s*x1) (s*x2)) (Vector (s*x2) (s*y2)) (Vector (s*x3) (s*y3))

scale :: Double -> Picture -> Picture
scale s (Picture es) = Picture $ Prelude.map (scaleElement s) es 

-- TODO rotate :: (Double, Double) -> Double -> Picture -> Picture

-- Examples

triangleCube :: Double -> Picture
triangleCube sideLen = Picture [
    (Rectangle (Vector 0 0) sideLen sideLen),
    (
        Triangle (Vector 0 sideLen)
        (Vector sideLen sideLen)
        (Vector (sideLen/2) 0)
        )
    ]

house :: Double -> Picture
house sideLen = Picture [
        t,
        (Circle (triangleCenter t) (sideLen/4)),
        (Rectangle (Vector 0 sideLen) sideLen sideLen)
    ]
    where t = Triangle (Vector (sideLen/2) 0) (Vector 0 sideLen) (Vector sideLen sideLen)

myLine = Line (Vector 0 0) (Vector 100 100)
myRektangle = Rectangle (Vector 0 0) 20 20
myTriangle = Triangle (Vector 10 10) (Vector 0 20) (Vector 60 20)
myCircle = Circle (Vector 50 50) 10

myPicture = [myLine, myRektangle, myTriangle, myCircle]

-- TODO dragon :: Int -> Picture

-- Output

svg = doctype <> with (svg11_ (drawPicture (Exercise03.VectorGraphics.scale 0.5 (house 50)))) [Width_ <<- "100", Height_ <<- "100"]
mainSVG = renderToFile "output.svg" svg
