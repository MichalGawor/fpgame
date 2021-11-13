{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
module Objects.Objects where



import Kinematics
import Plane

-- # Shared types
type Damage = Float
type Hp = Float


-- # Object classes
-- # every object can be moved
class Moveable a where
    move :: a -> Target Point -> Maybe a

-- # every object can be rendered
class Renderable a where
    render :: a -- #TODO

-- # some objects can be despawned
class Killable a where 
    takeDamage :: a -> Damage -> Maybe a
