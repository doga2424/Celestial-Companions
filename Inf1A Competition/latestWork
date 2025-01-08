import Graphics.Gloss
import Graphics.Gloss.Juicy (loadJuicyPNG)
import Data.Maybe (fromMaybe)

-- Object and Simulation Data Types
data Object = Object {position :: (Float, Float), radius :: Float, speed :: Float, time :: Float}
data Simulate = Simulate {passed :: Float, objects :: [Object]}

-- Load the Background
background :: IO Picture
background = do
    maybePic <- loadJuicyPNG "background.jpg"
    return $ fromMaybe blank maybePic

-- Initial Object States
start :: [Object]
start = [Object (0, 0) 100 1 0, Object (0, 0) 150 0.5 0]

-- Simulation State
starts :: Simulate
starts = Simulate 0 start

-- Function to Compute Positions Along a Circular Path
circularPath :: Float -> Float -> Float -> (Float, Float)
circularPath radius speed time = (x, y)
  where
    angle = time * speed
    x = radius * cos angle
    y = radius * sin angle

-- Update Function for Animation
update :: ViewPort -> Float -> Simulate -> Simulate
update _ dt (Simulate t objs) = Simulate (t + dt) (map updateObject objs)
  where 
    updateObject :: Object -> Object
    updateObject obj@(Object _ r s elapsedTime) = 
      let newTime = elapsedTime + dt
          (x, y) = circularPath r s newTime
      in obj { position = (x, y), time = newTime }

-- Render Function to Display the Background and Moving Objects
render :: Picture -> Simulate -> Picture
render bg (Simulate _ objs) = Pictures (bg : map drawObject objs)
  where
    drawObject :: Object -> Picture
    drawObject (Object (x, y) _ _ _) = translate x y $ color red $ circleSolid 10

-- Main Function to Load Background and Start the Simulation
main :: IO ()
main = do
    bg <- background
    simulate
        (InWindow "Moving Light with Background" (800, 600) (10, 10))  -- Window Configuration
        black                                                          -- Background Color
        60                                                             -- Frames per Second
        starts                                                         -- Initial State
        (render bg)                                                    -- Render Function
        update                                                         -- Update Function
