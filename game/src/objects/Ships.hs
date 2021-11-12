{-# LANGUAGE NamedFieldPuns #-}
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
    takeDamage ship@(Ship { currHp, position, collisionDamage }) damage | currHp > damaga = let newHp = currHp - damage 
                                                                                                in Just ship{ currHp = newHp, position = position, collisionDamage = collisionDamage }
                                                                        | otherwise = Nothing -- DEAL AREA DAMAGE HERE? EVENT HANDLING? 

-- # Player's Ship
data PlayerShip = MkPlayerShip Ship 

-- # Enemy datatype
data Enemy = MkSuicideEnemy SuicideShip | MkGunEnemy GunShip | MkRocketEnemy MkRocketShip deriving (Moveable, Renderable, Killable)

-- ## Enemy ships

-- ## Gun ship
data GunShip = MkGunShip Ship deriving (Killable)

instance Moveable GunShip where
    move :: GunShip -> Target -> Maybe GunShip
    move (MkGunShip ship@(Ship {position, velocity})), _) _ = MKGunShip ship{ position = uniformLinearMotion position velocity, velocity = velocity }


-- ## Rocket ship
data RocketShip = MkRocketShip Ship deriving (Killable)

instance Moveable RocketShip where
    move :: RocketShip -> Target -> Maybe RocketShip
    move (MkRocketShip ship@(Ship {position, velocity})) _ = uniformLinearMotion position velocity


-- ## Suicide ship
data SuicideShip = MkSuicideShip Ship AngularSpeed deriving (Killable)

instance Moveable SuicideShip where
    move :: SuicideShip -> Target -> Maybe SuicideShip
    move (MkSuicideShip ship@(Ship { position, velocity}), _) NoTarget = let 
                                                                        newPosition = uniformLinearMotion position velocity
                                                                        in (MkSuicideShip ship{ position = newPosition, velocity = velocity }, maxAngularSpeed)
    move (MkSuicideShip ship@(Ship { position, velocity}), maxAngularSpeed) (MkTarget (x, y)) = let 
                                                                                                      homingTrajectory = homingMotion position velocity maxAngularSpeed (x, y)
                                                                                                      newVelocity = homingTrajectory
                                                                                                      newPosition = uniformLinearMotion position homingTrajectory 
                                                                                                      in (MkSuicideShip ship{ position = newPosition, velocity = newVelocity }, maxAngularSpeed)


