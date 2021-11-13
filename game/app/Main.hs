{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
module Main where



import Data.Angle
import Objects.Projectiles.Weapon
import Objects.Ships
import Plane

main :: IO ()
main = do 
        putStrLn (show (MkSuicideShip testShip (Degrees (fromIntegral 5))))


testShip :: Ship
testShip = Ship 100 100 NoWeapon (50, 50)


