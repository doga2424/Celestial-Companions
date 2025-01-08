import Graphics.Gloss
import Graphics.Gloss.Juicy (loadJuicyPNG)
import Data.Maybe (fromMaybe)
import Graphics.Gloss.Data.ViewPort (ViewPort)

-- Define Object and Simulate data types
data Object = Object { position :: (Float, Float), angle :: Float, size :: Float }
data Simulate = Simulate { passed :: Float, objects :: [Object], combined :: Bool }

-- Load background image
background :: IO Picture
background = do
    maybePic <- loadJuicyPNG "background.jpg"
    return $ fromMaybe blank maybePic

-- Initial object states
start :: [Object]
start = [Object (0, 0) 0.0 1.0]  -- Starting at center with initial size
i
starts :: Simulate
starts = Simulate 0 start False

-- Calculate the position of an object on a circular path based on angle
circularPath :: Float -> Float -> (Float, Float)
circularPath radius angle = (x, y)
  where
    x = radius * cos angle
    y = radius * sin angle

-- Update function for animation
update :: ViewPort -> Float -> Simulate -> Simulate
update _ dt sim@(Simulate t objs combined) =
    if t < 10
    then Simulate (t + dt) (map (updateObject dt) objs) combined
    else combineAndGrow sim
  where 
    radius = 100.0  -- Radius of the circular path
    angularVelocity = 1.0  -- Angular velocity in radians per second

    updateObject :: Float -> Object -> Object
    updateObject dt obj@(Object _ angle size) =
      let newAngle = angle + angularVelocity * dt  -- Update angle based on angular velocity
          (x, y) = circularPath radius newAngle  -- Calculate new position based on updated angle
          newSize = size + 0.1  -- Gradually increase the size
      in Object (x, y) newAngle newSize  -- Update position, angle, and size

    combineAndGrow :: Simulate -> Simulate
    combineAndGrow (Simulate t objs _) =
      let combinedSize = sum (map size objs) + (t - 10) * 5  -- Combine sizes and continue growing
          center = (0, 0)  -- Center position for the combined object
      in Simulate t [Object center combinedSize combinedSize] True  -- Create one combined object

-- Render function to display background and objects
render :: Picture -> Simulate -> Picture
render bg (Simulate current objs _) = Pictures [bg, Pictures (map (drawObject current) objs)]
  where
    drawObject :: Float -> Object -> Picture
    drawObject cu (Object (x, y) _ size) = translate x y $ color red $ circleSolid size

-- Main function to load background and start the simulation
main :: IO ()
main = do
    bg <- background
    simulate
        (InWindow "Moving Light with Background" (800, 600) (10, 10))
        black
        45
        starts
        (render bg)
        update
