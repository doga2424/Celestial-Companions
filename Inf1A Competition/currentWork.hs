import Graphics.Gloss
import Graphics.Gloss.Juicy (loadJuicyPNG)
import System.Random (randomRIO)
import System.IO.Unsafe (unsafePerformIO)
import Data.Maybe (fromMaybe)
import Graphics.Gloss.Data.ViewPort (ViewPort)


data Object = Object {position :: (Float, Float), velocity :: (Float, Float), time :: Float}
data Simulate = Simulate {passed :: Float, objects :: [Object]}

background = do
    maybePic <- loadJuicyPNG "background.jpg"
    return $ fromMaybe blank maybePic  -- Use `fromMaybe` directly



start :: [Object]
start = [Object (-100, -100) (80, 120) 1.5, Object (100, 100) (-80, -120) 5.0]

starts :: Simulate
starts = Simulate 0 start

circularPath :: Float -> Float -> Float -> (Float, Float)
circularPath radius speed time = (x, y)
  where
    angle = time * speed
    x = radius * cos angle
    y = radius * sin angle

circularPathWithRandomRadius :: Float -> Float -> Float -> IO (Float, Float, Float)
circularPathWithRandomRadius speed time previousRadius = do
    let angle = speed * time
    let completeCircles = floor (angle / (2 * pi))

    newRadius <- if completeCircles > 0
                 then randomRIO (30, 70)
                 else return previousRadius
    return (newRadius, newRadius * cos angle, newRadius * sin angle)

update :: ViewPort -> Float -> Simulate -> Simulate
update _ dt (Simulate t objs) = Simulate (t + dt) (map (updateObject (t + dt)) objs)
  where 
    updateObject :: Float -> Object -> Object
    updateObject dt obj@(Object (x, y) (vx, vy) t)
      | passed >= t = Object (x + vx * dt, y + vy * dt) (vx, vy) t
      | otherwise = obj
      where passed = dt

frame :: Float -> IO Picture -> Picture
frame time backgroundIO = Pictures [backgroundLayer, movingLightLayer]
  where
    backgroundLayer = unsafePerformIO backgroundIO
    movingLightLayer = translate x y $ color red $ circleSolid 10
    (x, y) = circularPath 50 1 time

pink = makeColorI 255 182 193 255

render :: IO Picture -> Simulate -> Picture
render bg (Simulate current objs) = Pictures [bgPic, Pictures (map (drawObject current) objs)]
  where
    bgPic = unsafePerformIO bg
    drawObject :: Float -> Object -> Picture
    drawObject cu (Object (x, y) _ t)
      | cu >= t = translate x y $ color pink $ circleSolid 2
      | otherwise = blank

main :: IO ()
main = do
    bg <- background
    simulate
        (InWindow "Moving Light" (800, 600) (10, 10))
        black
        45
        starts
        (render (return bg))
        update
