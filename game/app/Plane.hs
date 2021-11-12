module Plane where

import Graphics.Gloss (Point)
import Data.Angle (Degrees, arccosine, cosine, sine)


-- ### Types
type HitBox = (Point, Point) -- square hitbox
type PolarVector = (Float, (Degrees Float))
data Target = MkTarget Point | NoTarget
type Vector = Point

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
                               in arccosine cosA 

vecToPolar :: Vector -> PolarVector
-- represent vector in polar coordinater
vecToAngular (x, y) = let magnitude = (sqrt (x ** 2 + y ** 2))
                          in MkAngularVector magnitude arccosine (x/magnitude)

polarToVec :: PolarVector -> Vector
-- represent polar coords as a vector
polarToVec (radius, (Degrees angle)) = (radius * cosine angle, radius * sine angle)

polarVecAddAngle :: PolarVector -> Degrees Float -> PolarVector
-- move polar by angle
polarVecAddAngle (radius, (Degrees angle)) (Degrees deltaAngle) = radius (Degree (angle + deltaAngle)) 

-- ### Screen control
isInScreen :: Position -> Bool
isInScreen (x, y) | x > ScreenWidth || x < 0 || y > ScreenWidth || y < 0 = True
                  | otherwise = False


