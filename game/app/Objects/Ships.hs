{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveAnyClass #-}
module Objects.Ships where 

import Data.Angle 
import Graphics.Gloss

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
    collisionDamage :: Damage,
    getColor :: Color,
    getPicture :: Picture }

instance Killable Ship where
    takeDamage :: Ship -> Damage -> Maybe Ship
    takeDamage ship@(Ship { currHp, position, collisionDamage }) damage | currHp > damage = let newHp = currHp - damage 
                                                                                                in Just ship{ currHp = newHp, position = position, collisionDamage = collisionDamage }
                                                                        | otherwise = Nothing -- DEAL AREA DAMAGE HERE? EVENT HANDLING? 


instance Renderable Ship where
    render Ship { position, getColor, getPicture } = uncurry Graphics.Gloss.translate position (color getColor getPicture)


-- # Player's Ship
data PlayerShip = MkPlayerShip Ship

instance Moveable PlayerShip where
    move :: PlayerShip -> Target Point -> Maybe PlayerShip
    move pShip@(MkPlayerShip ship@(Ship { position })) (MkTarget (x, y)) | x == 1 = case uniformLinearMotion position (1, 0) of
                                                                                    newPosition | isInScreen newPosition -> Just (MkPlayerShip ship{position=newPosition})
                                                                                                | otherwise -> Just (MkPlayerShip ship)
                                                                         | x == -1 =case uniformLinearMotion position (-1, 0) of
                                                                                    newPosition | isInScreen newPosition -> Just (MkPlayerShip ship{position=newPosition})
                                                                                                | otherwise -> Just (MkPlayerShip ship)

instance Renderable PlayerShip where
    render (MkPlayerShip ship) = render ship                                                                  

-- # Enemy datatype
data Enemy = MkSuicideEnemy SuicideShip | MkGunEnemy GunShip | MkRocketEnemy RocketShip | MkExplosion Time deriving (Moveable)-- deriving (Moveable, Renderable, Killable)

instance Renderable Enemy where
    render (MkSuicideEnemy suicideShip) = render suicideShip
    render (MkGunEnemy gunShip) = render gunShip
    render (MkRocketEnemy rocketShip) = render rocketShip
    render (MkExplosion time) = undefined

-- instance Moveable Enemy where
--     move (MkSuicideEnemy suicideShip) target = MkSuicideShip (move suicideShip target)
--     move (MkGunEnemy gunShip) target = MkGunEnemy (move gunShip target)
--     move (MkRocketEnemy rocketShip) target = MkRocketEnemy (move rocketShip target)
--     move (MkExplosion time) = undefined

-- ## Enemy ships
-- ## Gun ship
data GunShip = MkGunShip Ship -- deriving (Killable)

instance Moveable GunShip where
    move :: GunShip -> Target Point -> Maybe GunShip
    -- moves independent of the target, TODO keep distance on horizontal
    move (MkGunShip ship@( Ship { position, velocity })) _ = case uniformLinearMotion position velocity of 
                                                                 newPosition | isInScreen newPosition -> Just (MkGunShip ship{ position=newPosition, velocity=velocity })         
                                                                             | otherwise -> Nothing                                             

instance Renderable GunShip where
    render (MkGunShip ship) = render ship

-- ## Rocket ship
data RocketShip = MkRocketShip Ship -- deriving (Killable)

instance Moveable RocketShip where 
    move :: RocketShip -> Target Point -> Maybe RocketShip
    move (MkRocketShip ship@(Ship {position, velocity})) _ = case uniformLinearMotion position velocity of
                                                                 newPosition | isInScreen newPosition -> Just (MkRocketShip ship{ position=newPosition, velocity=velocity})
                                                                             | otherwise -> Nothing
                                                            
instance Renderable RocketShip where
    render (MkRocketShip ship) = render ship


-- ## Suicide ship
data SuicideShip = MkSuicideShip Ship (Degrees Float) -- deriving (Killable)

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

instance Renderable SuicideShip where
    render (MkSuicideShip ship _) = render ship

-- TESTING
instance Show Ship where
    show :: Ship -> String
    show ship@( Ship{ position }) = show position




-- # SHIPS READY TO USE!
baseShip :: Ship
baseShip = Ship {maxHp=100, currHp=100, weapon=NoWeapon, position=(50, 50), velocity=(-2.0, 0.0), collisionDamage=20, getColor=black, getPicture=(square 2.0) }

suicideShip :: SuicideShip
suicideShip = MkSuicideShip baseShip{ getColor=red, getPicture=circle 2.0} (Degrees 5)

playerShip :: PlayerShip
playerShip = MkPlayerShip baseShip{ getColor=cyan, getPicture=rectangle 4.0 2.0, velocity=(0.0, 0.0)}




-- # SHAPES FOR SHIPS
square :: Float -> Picture
square side = rectangleSolid side side

rectangle :: Float -> Float -> Picture
rectangle sideA sideB = rectangleSolid sideA sideB

