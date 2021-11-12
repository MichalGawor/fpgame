module Main where


import Data.Angle (Degrees)

import Ships
import Plane

main :: IO ()
main = rednder (MkSuicideShip) (Deegres 5) 


testShip :: Ship
testShip = MkShip 100 100 NoWeapon (50, 50)


