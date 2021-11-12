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
data Projectile = MkBulletProjectile Bullet | MkRocketProjectile Rocket | MkBeamProjectile Beam deriving (Moveable)

-- ## Projectiles 
-- ## Bullet 
data Bullet = MkBullet Position Velocity Damage

-- ## Rocket 
data Rocket = MkRocket Position Velocity Damage AngularSpeed Time

instance Killable Rocket where
    takeDamage :: Rocket -> Damage -> Maybe Rocket 
    takeDamage (MkRocket position velocity damage angularSpeed time) = 

instance Moveable Rocket where
    move :: Rocket -> Target -> Maybe Rocket
    -- if no target move straight forward
    move rocket@{ position, velocity } NoTarget = let newPosition = uniformLinearMotion position velocity
                                                                   in rocket{ position=newPosition, velocity=velocity }
    -- otherwise track the target
    move rocket@{ position, velocity, angularSpeed}
    
    -- otherwise track the target
instance Moveable SuicideShip where
    move :: SuicideShip -> Target -> Maybe SuicideShip
    move (MkSuicideShip ship@(Ship { position, velocity}), _) NoTarget = let 
                                                                             newPosition = uniformLinearMotion position velocity
                                                                             in MkSuicideShip ship{ position = newPosition, velocity = velocity } maxAngularSpeed
    move (MkSuicideShip ship@(Ship { position, velocity}), maxAngularSpeed) (MkTarget (x, y)) = let 
                                                                                                      homingTrajectory = homingMotion position velocity maxAngularSpeed (x, y)
                                                                                                      newVelocity = homingTrajectory
                                                                                                      newPosition = uniformLinearMotion position homingTrajectory 
                                                                                                      in (MkSuicideShip ship{ position = newPosition, velocity = newVelocity }, maxAngularSpeed)




-- ## Beam ON HOLD, MAYBE IN FUTURE
data Beam = MkBeam Position Width Damage Time (Shootable, Positioned, Renderable)