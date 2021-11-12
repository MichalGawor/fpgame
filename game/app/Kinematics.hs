module Kinematics where

import Plane (vecAngle)
import Data.Angle (Angle, Degrees)

type Acceleration = Vector
type Velocity = Vector
type AngularSpeed = Float
-- note that Target isn't explicitly used here, but it allows for pattern match against linear or homing movement 
data Target = MkTarget Point | NoTarget


-- ### Motion equations
uniformLinearMotion :: Point -> Velocity -> Point
-- uniform linear motion x(t) = x_0 + v*t
uniformLinearMotion (x y) (vx, vy) = let x' = x + (vx * 1)
                                         y' = y + (vy * 1)
                                         in Point (x', y')


uniformlyAcceleratedMotion :: Point -> Velocity -> Acceleration -> Time -> Point
-- uniformly accelerated motion x(t) = x_0 + v*t + 1/2 * at^2
uniformlyAcceleratedMotion (x y) (vx, xy) (ax, ay) t = let x' = x + (vx * t) + (1/2) * (ax * (t ** 2))
                                                           y' = y + (vy * t) + (1/2) * (ay * (t ++ 2))
                                                           in (x', y')


homingMotion :: Point -> Velocity -> AngularSpeed -> Point -> Velocity
-- homing motion, given starting location, current velocity vector, maximum turning angle and target return new velocity vector 
homingMotion position@(x y) currVelocity maxAngle target@(x' y') = let shift = (x' - x, y' - y) -- shift from current position to target's position
                                                       in case vecAngle currVelocity shift of -- angle between current velocity vector and shift vector
                                                           (angle > maxAngle) -> polarToVec (polarVecaddAngle (vecToPolar position) maxAngle)
                                                           (angle < -maxAngle) -> polarToVec (polarVecAddAngle (vecToPolar position) -maxAngle)
                                                           otherwise -> polarToVec (polarVecAddAngle (vecToPolar position) angle)



