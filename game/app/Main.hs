{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UnicodeSyntax #-}
module Main where



import Data.Angle

import Kinematics
import Objects.Objects
import Objects.Projectiles.Weapon
import Objects.Ships
import Plane



main :: IO ()
main = putStrLn (show testSuicideShip)

testSuicideShip :: SuicideShip
testSuicideShip = MkSuicideShip testShip (Degrees (fromIntegral 5))

testShip :: Ship
testShip = Ship {maxHp=100, currHp=100, weapon=NoWeapon, position=(50, 50), velocity=(2, 0), collisionDamage=20}

testGunShip :: GunShip 
testGunShip = MkGunShip testShip

noTarget :: Target Point
noTarget = NoTarget

