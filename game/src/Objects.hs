module Objects
    ( 
    ) where

{-# LANGUAGE NamedFieldPuns #-}




-- ### Weapons
type RPM = Int -- rounds per minute
data Weapon = MkWeapon Projectile Damage RPM | NoWeapon 
data Projectile = MkBulletProjectile Bullet | MkRocketProjectile Rocket | MkBeamProjectile Beam
data Bullet = MkBullet Position Velocity Damage deriving (Shootable, Positioned, Renderable, Moveable)
data Rocket = MkRocket Position Velocity Damage AngularSpeed Time deriving (Shootable, Positioned, Renderable, Moveable)
data Beam = MkBeam Position Width Damage Time (Shootable, Positioned, Renderable)

-- ### Ships
data Ship = Ship { 
    maxHp :: Hp,
    currHp :: Hp,
    weapon :: Weapon,
    position :: Point,
    velocity :: Velocity,
    collisionDamage :: Damage }

-- # Player's Ship
data PlayerShip = MkPlayerShip Ship deriving (Renderable, Moveable, Killable)

-- # Enemy ships
data SuicideShip = MkSuicideShip Ship AngularSpeed deriving (Renderable, Moveable, Killable)
data GunShip = MkGunShip Ship deriving (Renderable, Moveable, Killable)
data RocketShip = MkRocketShip Ship deriving (Renderable, Moveable, Killable)

-- # Enemy datatype
data Enemy = MkSuicideEnemy SuicideShip | MkGunEnemy GunShip | MkRocketEnemy MkRocketShip

instance Moveable SuicideShip where
    move :: SuicideShip -> Target -> Maybe SuicideShip
    move (MkSuicideShip (Ship { position, speed, velocity}), _) NoTarget = uniformLinearMotion position velocity 
    move (MkSuicideShip (Ship { position, speed, velocity}), maxAngularSpeed) (MkTarget (x, y)) = let 
                                                                                                      homingTrajectory = homingMotion position velocity maxAngularSpeed (x, y)
                                                                                                      in uniformLinearMotion position homingTrajectory 

    movementSpeed :: SuicideShip -> SuicideShip
    movementSpeed ()
    