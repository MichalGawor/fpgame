module Main where


import Data.Angle (Degree)

import Ships
import Plane

main :: IO ()
main = rednder (MkSuicideShip) (Deegres 5) 


testShip :: Ship
testShip = MkShip 100 100 NoWeapon (50, 50)


screenWidth :: ScreenWidth
screenWidth = 800

screenHeight :: ScreenHeight
screenHeight = 640