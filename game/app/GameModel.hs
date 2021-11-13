module GameState where

data WorldState = GameState
 { player :: [PlayerShip]
 , enemies :: [Enemy]
 , projectiles :: [Projectiles]
 }

