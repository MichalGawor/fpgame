{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}

module Plane where

import Data.Angle
-- ### Types
type HitBox = (Point, Point) -- square hitbox
type PolarVector = (Float, (Degrees Float))
type Vector = Point
type Point = (Float, Float)
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
vecToPolar (x, y) = let magnitude = (sqrt (x ** 2 + y ** 2))
                          in (magnitude, (arccosine (x/magnitude)))

polarToVec :: PolarVector -> Vector
-- represent polar coords as a vector
polarToVec (radius, angle) = (radius * (cosine angle), radius * (sine angle))

polarVecAddAngle :: PolarVector -> Degrees Float -> PolarVector
-- move polar by angle
polarVecAddAngle (radius, Degrees angle) (Degrees deltaAngle) = (radius, (Degrees (angle + deltaAngle))) 

-- ### Screen control
isInScreen :: Point -> Bool
isInScreen (x, y) | x > screenWidth || x < 0 || y > screenWidth || y < 0 = True
                  | otherwise = False

screenWidth :: ScreenWidth
screenWidth = 800

screenHeight :: ScreenHeight
screenHeight = 640
