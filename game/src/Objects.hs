module Objects
    ( 
    ) where

{-# LANGUAGE NamedFieldPuns #-}




-- ### Weapons
type RPM = Int -- rounds per minute
data Weapon = MkWeapon Projectile RPM | NoWeapon 
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
data SuicideShip = MkSuicideShip Ship AngularSpeed deriving (Renderable, Killable)
data GunShip = MkGunShip Ship deriving (Renderable, Killable)
data RocketShip = MkRocketShip Ship deriving (Renderable, Killable)

-- # Enemy datatype
data Enemy = MkSuicideEnemy SuicideShip | MkGunEnemy GunShip | MkRocketEnemy MkRocketShip


-- # Moveable objects
instance Moveable SuicideShip where
    move :: SuicideShip -> Target -> Maybe SuicideShip
    move (MkSuicideShip sShip@(Ship { position, velocity}), _) NoTarget = let 
                                                                        newPosition = uniformLinearMotion position velocity
                                                                        in (MkSuicideShip sShip{ position = newPosition, velocity = velocity }, maxAngularSpeed)
    move (MkSuicideShip sShip@(Ship { position, velocity}), maxAngularSpeed) (MkTarget (x, y)) = let 
                                                                                                      homingTrajectory = homingMotion position velocity maxAngularSpeed (x, y)
                                                                                                      newVelocity = homingTrajectory
                                                                                                      newPosition = uniformLinearMotion position homingTrajectory 
                                                                                                      in (MkSuicideShip sShip{ position = newPosition, velocity = newVelocity }, maxAngularSpeed)


instance Moveable GunShip where
    move :: GunShip -> Target -> Maybe GunShip
    move (MkGunShip (Ship {position, velocity})), _) _ = uniformLinearMotion position velocity

instance Moveable RocketShip where
    move :: RocketShip -> Target -> Maybe RocketShip
    move (MkRocketShip (Ship {position, velocity}))

-- # Shootable
instance Shootable Weapon where
    shoot :: Weapon -> Maybe Projectile
    shoot NoWeapon = Nothing
    shoot (MkWeapon (MkBeam _ _ _ _)) = Nothing -- #TODO MAYBE 
    shoot (MkWeapon projectile rpm) = projectile -- #TODO MUST fire rate handling

instance Killable Ship where
    takeDamage ship damage :: Ship -> Damage -> Maybe Ship
    takeDamage ship@(Ship { currHp, position, collisionDamage }) damage | currHp > damaga = let newHp = currHp - damage 
                                                                                                in Just ship{ currHp = newHp, position = position, collisionDamage = collisionDamage }
                                                                        | isInScreen = Nothing
                                                                        | otherwise = Nothing -- DEAL AREA DAMAGE HERE 
