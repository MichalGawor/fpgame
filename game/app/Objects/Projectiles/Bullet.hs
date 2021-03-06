{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
module Objects.Projectiles.Bullet where 

import Kinematics
import Plane
import Objects.Objects



-- ## Bullet 
data Bullet = MkBullet { position::Point,
                         velocity::Velocity, 
                         damage::Damage }

instance Moveable Bullet where
    move :: Bullet -> Target Point -> Maybe Bullet
    -- behaves independent on the targets due to innertia of the bullet 
    move bullet@(MkBullet { position, velocity }) _ = case uniformLinearMotion position velocity of
                                                          newPosition | isInScreen newPosition -> Just bullet{ position=position, velocity=velocity}
                                                          otherwise -> Nothing

instance Killable Bullet where 
    -- just so Projectile can derive Killable
    takeDamage :: Bullet -> Damage -> Maybe Bullet
    takeDamage bullet _ = Just bullet