module Projectiles where

import Objects
import Plane
{-# LANGUAGE NamedFieldPuns #-}
-- # Basic types
type RPM = Int -- rounds per minute

-- # Weapons
data Weapon = MkWeapon Projectile RPM | NoWeapon 

instance Shootable Weapon where
    shoot :: Weapon -> Maybe Projectile
    shoot NoWeapon = Nothing
    shoot (MkWeapon (MkBeam _ _ _ _)) = Nothing -- #TODO MAYBE 
    shoot (MkWeapon projectile rpm) = Just projectile -- #TODO MUST fire rate handling

-- # Projectile
data Projectile = MkBulletProjectile Bullet | MkRocketProjectile Rocket deriving (Moveable, Killable)

-- ## Projectiles 
-- ## Bullet 
data Bullet = MkBullet { position::Position,
                         velocity::Velocity, 
                         damage::Damage}

instance Moveable Bullet where
    move :: Bullet -> Target -> Maybe Bullet
    -- behaves independent on the targets due to innertia of the bullet 
    move bullet@{ position, velocity} _ = let newPosition = uniformLinearMotion position velocity
                                              in bullet{ position=position, velocity=velocity}

instance Killable Bullet where 
    -- just so Projectile can derive Killable
    takeDamage :: Bullet -> Damage -> Maybe Bullet
    takeDamage bullet _ = Just bullet

-- ## Rocket 
data Rocket = MkRocket {
    position :: Position 
    velocity :: Velocity 
    damage :: Damage 
    maxAngularSpeed :: AngularSpeed 
    existingTime :: Time
}

instance Killable Rocket where
    takeDamage :: Rocket -> Damage -> Maybe Rocket 
    takeDamage rocket dealDamage | dealDamage < 1 = Nothing
                                 | otherwise = Just rocket 

instance Moveable Rocket where
    move :: Rocket -> Target -> Maybe Rocket
    -- if no target move straight forward
    move rocket@{ position, velocity } NoTarget = let newPosition = uniformLinearMotion position velocity
                                                      in rocket{ position=newPosition, velocity=velocity }
    -- otherwise track the target
    move rocket@{ position, velocity, maxAngularSpeed} (MkTarget (x, y)) = let homingTrajectory = homingMotion position velocity maxAngularSpeed (x, y)
                                                                               newVelocity = homingTrajectory
                                                                               newPosition = uniformLinearMotion position homingTrajectory
                                                                               in rokcet{ position=newPosition, velocity=newVelocity, maxAngularSpeed=maxAngularSpeed }

-- ## Beam ON HOLD, MAYBE IN FUTURE
data Beam = MkBeam Position Width Damage Time (Shootable, Positioned, Renderable)
