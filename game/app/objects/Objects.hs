module Objects where

-- # Object classes
-- # every object can be moved
class Moveable a where
    move :: a -> Target -> Maybe a

-- # every object can be rendered
class Renderable a where
    render :: a -- #TODO

-- # some objects can be despawned
class Killable a where 
    takeDamage :: a -> Damage -> Maybe a

-- # some objects can produce projectile
class Shootable a where
    shoot :: a -> Maybe Projectile

