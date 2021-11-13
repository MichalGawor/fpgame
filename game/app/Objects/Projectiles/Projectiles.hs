{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}

module Objects.Projectiles.Projectiles where

import Plane
import Objects.Projectiles.Bullet
import Objects.Projectiles.Rocket

-- # Basic types

data Projectile = MkPojectileBullet Bullet | MkProjectileRocket Rocket




-- ## Projectiles 


