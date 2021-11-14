{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UnicodeSyntax #-}
module Main where



import Data.Angle

import MVC.Controller
import MVC.GameModel
import MVC.View

import Graphics.Gloss.Interface.Pure.Simulate
import Kinematics
import Objects.Objects
import Objects.Projectiles.Weapon
import Objects.Ships
import Plane



-- main :: IO ()
-- main = do 
--         putStrLn ""
--             do 
--             movedShip <- move testSuicideShip (noTarget)
--             putStrLn (show (movedShip))

-- testSuicideShip :: SuicideShip
-- testSuicideShip = MkSuicideShip testShip (Degrees (fromIntegral 5))

-- testShip :: Ship
-- testShip = Ship {maxHp=100, currHp=100, weapon=NoWeapon, position=(50, 50), velocity=(2, 0), collisionDamage=20}

-- testGunShip :: GunShip 
-- testGunShip = MkGunShip testShip

-- noTarget :: Target Point
-- noTarget = NoTarget

import Graphics.Gloss.Interface.IO.Game

fps :: Int
fps = 30

width, height, offset :: Int
width = 300
height = 300
offset = 100

background :: Color
background = black

window :: Display
window = InWindow "Shoot'em All" (width, height) (offset, offset)

main :: IO ()
main = simulate window background fps initialGameState MVC.View.renderGame MVC.Controller.update