module Plane
    ( Vector ScreenHeight, ScreenWidth, Resoulution
    ) where

import Graphics.Gloss (Point)
import Data.Angle (Angle, Degree, arccosine, cosine, sine)


-- ### Types
type Vector = Point
type HitBox = (Point, Point) -- square gitbox
type PolarVector = Float (Degree Float)

type ScreenWidth = Float -- Gloss uses Floats 
type ScreenHeight = Float -- Gloss uses Floats
type Resoulution = (ScreenWidth, ScreenHeight)

-- ### Vector operations
translate :: Vector -> Point -> Point
translate (x, y) (a, b) = (a + x, b + y)

-- ### Polar representation

vecAngle :: Vector -> Vector -> Degrees Float
-- get angle between two vectors in degrees
vecAngle (x, y) (x', y') = let cosA = ((x * x') + (y * y')) / ((sqrt (x ** 2 + y ** 2)) * (sqrt (x' ** 2 + y' ** 2)))
                             alpha = arccosine cosA 

vecToPolar :: Vector -> PolarVector
-- represent vector in polar coordinater
vecToAngular (x, y) = let magnitude = (sqrt (x ** 2 + y ** 2))
    MkAngularVector magnitude arccosine (x/magnitude)

polarToVec :: PolarVector -> Vector
polarToVec radius (Degree angle) = (radius * cosine angle, radius * sine angle)

polarVecAddAngle :: PolarVector -> Degrees Float -> PolarVector
polarVecAddAngle (radius (Degree angle)) (Degree deltaAngle) = radius (Degree (angle + deltaAngle)) 

-- ### Screen control
isInScreen :: Position -> Bool
isInScreen (x y) | x > ScreenWidth || x < 0 || y > ScreenWidth || y < 0 = True
                 | otherwise = False