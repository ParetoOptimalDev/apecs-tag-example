{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Apecs
import Apecs.Gloss
import Linear
import Prelude hiding (Down, Map, get, modify)

newtype Position = Position (V2 Float) deriving newtype (Ord, Eq, Num, Show)
instance Component Position where type Storage Position = Map Position

data Player = Player deriving stock (Show)
instance Component Player where type Storage Player = Unique Player

data AI1 = AI1 deriving stock (Show)
instance Component AI1 where type Storage AI1 = Unique AI1

newtype Time = Time Float deriving newtype (Show, Num)
instance Semigroup Time where (<>) = (+)
instance Monoid Time where mempty = 0
instance Component Time where type Storage Time = Global Time

newtype DebugPosition = DebugPosition String deriving newtype (Show, Semigroup, Monoid)
instance Component DebugPosition where type Storage DebugPosition = Global DebugPosition

makeWorld "World" [''Position, ''Player, ''AI1, ''Time, ''Camera, ''DebugPosition]

type System' a = System World a

render :: System' Picture
render = do
  player <- foldDraw $ \(Player, pos) -> translate' pos . color blue . circleSolid $ 30
  ai1 <- foldDraw $ \(AI1, pos) -> translate' pos . color red . Polygon $ rectanglePath 50 50

  DebugPosition dbp <- get global

  let initialDebugLabelPos = Position (V2 (-400) 350)
      debugFormat pos = color white . translate' pos . scale 0.25 0.25 . Text
      aiDebugLabelPos = Position (V2 (-400) 350)
      playerDebugLabelPos = aiDebugLabelPos + 100 -- (Position (V2 (- 400) (200)))
      -- debugAiPos = debugFormat aiDebugLabelPos ("AiPosition: " ++ show dbp)
      -- debugPlayerPos = debugFormat playerDebugLabelPos ("PlayerPosition: " ++ "TODO")
      genDebugPositions initial =
        [ Position (V2 (-400) 350)
        , Position (V2 (-400) 250)
        ]
      [debugPlayerPos, debugAiPos] =
        [ debugFormat aiDebugLabelPos ("AiPosition: " ++ show dbp)
        , debugFormat playerDebugLabelPos ("PlayerPosition: " ++ "TODO")
        ]

  pure $ player <> ai1 <> debugAiPos <> debugPlayerPos

-- Game Logic

stepIncrement :: Float
stepIncrement = 15

handleEvent :: Event -> SystemT World IO ()
handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) =
  cmap $ \(Player, Position (V2 x y)) -> Position (V2 (x - stepIncrement) y)
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) =
  cmap $ \(Player, Position (V2 x y)) -> Position (V2 (x + stepIncrement) y)
handleEvent (EventKey (SpecialKey KeyUp) Down _ _) =
  cmap $ \(Player, Position (V2 x y)) -> Position (V2 x (y + stepIncrement))
handleEvent (EventKey (SpecialKey KeyDown) Down _ _) =
  cmap $ \(Player, Position (V2 x y)) -> Position (V2 x (y - stepIncrement))
handleEvent _ = pass

triggerEvery :: Float -> Float -> Float -> System' a -> System' ()
triggerEvery dT period phase sys = do
  Time t <- get global
  let t' = t + phase
      trigger = floor (t' / period) /= floor ((t' + dT) / period)
  when trigger $ void sys

incrTime :: Float -> System' ()
incrTime dT = modify global $ \(Time t) -> Time (t + dT)

move :: forall {a} {f :: Type -> Type}. (Floating a, Metric f, Epsilon a, Ord a, Num (f a)) => a -> f a -> f a -> f a
move velocity current target =
  let vNorm = normalize (current - target)
   in if norm (current - target) > 69
        then current + vNorm ^* velocity
        else current

moveTowardsPlayer :: Position -> Position -> Position
moveTowardsPlayer (Position pv) (Position aiv) = do
  Position $ move (-4) aiv pv

-- TODO don't let players overlap and detect collisions to not allow moving to that (x,y)
stepAIPosition :: Float -> System' ()
stepAIPosition f =
  cmapM $ \(Player, playerPos) -> do
    cmapM $ \(AI1, debugPos) ->
      let pos = moveTowardsPlayer playerPos debugPos
       in do
            pure (pos, DebugPosition (show pos))

timeStep :: Float -> System' ()
timeStep dT = do
  incrTime dT
  stepAIPosition dT

-- triggerEvery dT 0.1 1 $ stepAIPosition dT

-- TODO make q exit the game
initialize = do
  playerEty <- newEntity (Player, Position initialPlayerPos)
  aiEty1 <- newEntity (AI1, Position initialAIPos)
  pass

main :: IO ()
main = do
  w <- initWorld
  runWith w $ do
    initialize
    play window (dim . dim . dim $ green) 10 render handleEvent timeStep

-- World Consts
initialPlayerPos = V2 100 100
initialAIPos = V2 0 0

worldSize :: Integer
worldSize = 500

window :: Display
window = InWindow "Tag" (600, 600) (10000, 10)

-- helpers
translate' :: Position -> Picture -> Picture
translate' (Position (V2 x y)) = translate x y
