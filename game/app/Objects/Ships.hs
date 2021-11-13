{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
module Objects.Ships where 

import Data.Angle (Degrees)

import Kinematics
import Plane
import Time
import Objects.Objects
import Objects.Projectiles.Weapon

-- # Ships
--      Ship data record stores data fields shared by all the ships and implements its mortality
data Ship = Ship { 
    maxHp :: Hp,
    currHp :: Hp,
    weapon :: Weapon,
    position :: Point,
    velocity :: Velocity,
    collisionDamage :: Damage }

instance Killable Ship where
    takeDamage :: Ship -> Damage -> Maybe Ship
    takeDamage ship@(Ship { currHp, position, collisionDamage }) damage | currHp > damage = let newHp = currHp - damage 
                                                                                                in Just ship{ currHp = newHp, position = position, collisionDamage = collisionDamage }
                                                                        | otherwise = Nothing -- DEAL AREA DAMAGE HERE? EVENT HANDLING? 

-- # Player's Ship
data PlayerShip = MkPlayerShip Ship 

-- # Enemy datatype
data Enemy = MkSuicideEnemy SuicideShip | MkGunEnemy GunShip | MkRocketEnemy RocketShip | MkExplosion Time -- deriving (Moveable, Renderable, Killable)

-- #
data ObjectOrExplosion a = MkObjectOrExplosion a | MkObjectOrExplosion (Float, Float, Time)

-- ## Enemy ships
-- ## Gun ship
data GunShip = MkGunShip Ship -- deriving (Killable)

instance Moveable GunShip where
    move :: GunShip -> Target Point -> Maybe GunShip
    -- moves independent of the target, TODO keep distance on horizontal
    move (MkGunShip ship@( Ship { position, velocity })) _ = case uniformLinearMotion position velocity of 
                                                                 newPosition | isInScreen newPosition -> Just (MkGunShip ship{ position=newPosition, velocity=velocity })         
                                                                             | otherwise -> Nothing                                             


-- ## Rocket ship
data RocketShip = MkRocketShip Ship -- deriving (Killable)

instance Moveable RocketShip where 
    move :: RocketShip -> Target Point -> Maybe RocketShip
    move (MkRocketShip ship@(Ship {position, velocity})) _ = case uniformLinearMotion position velocity of
                                                                 newPosition | isInScreen newPosition -> Just (MkRocketShip ship{ position=newPosition, velocity=velocity})
                                                                             | otherwise -> Nothing
                                                            



-- ## Suicide ship
data SuicideShip = MkSuicideShip Ship (Degrees Float) deriving (Show) -- deriving (Killable)

instance Moveable SuicideShip where
    -- moves straight if no target
    move :: SuicideShip -> Target Point -> Maybe SuicideShip
    move (MkSuicideShip ship@(Ship { position, velocity}) maxAngularSpeed) NoTarget = case uniformLinearMotion position velocity of
                                                                             newPosition | isInScreen newPosition -> Just (MkSuicideShip ship{ position = newPosition, velocity = velocity } maxAngularSpeed)
                                                                                         | otherwise -> Nothing
    -- track if target given
    move (MkSuicideShip ship@(Ship { position, velocity}) maxAngularSpeed) (MkTarget (x, y)) = let 
                                                                                                      homingTrajectory = homingMotion position velocity maxAngularSpeed (x, y)
                                                                                                      newVelocity = homingTrajectory
                                                                                                      newPosition = uniformLinearMotion position homingTrajectory 
                                                                                                      in case newPosition of
                                                                                                          newPosition | isInScreen newPosition -> Just (MkSuicideShip ship{ position = newPosition, velocity = newVelocity } maxAngularSpeed)
                                                                                                                      | otherwise -> Nothing



-- TESTING
instance Show Ship where
    show :: Ship -> String
    show ship@( Ship{ position }) = show position