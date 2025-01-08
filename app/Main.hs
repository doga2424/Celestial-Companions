module Main where

import HaskellSay (haskellSay)
import Graphics.Gloss
import Graphics.Gloss.Juicy (loadJuicyPNG)
import Data.Maybe (fromMaybe)
import Graphics.Gloss.Data.ViewPort (ViewPort)
import Graphics.Gloss.Interface.Pure.Simulate
import Data.Fixed (mod')

data Object = Object { position :: (Float, Float), angle :: Float, size :: Float, phaseShift :: Float, trail :: [(Float, Float)] }
data Simulate = Simulate { passed :: Float, objects :: [Object], combined :: Bool, combinedTime :: Float, growthDuration :: Float, flicker :: Bool }

-- Define pastel rainbow colors
pastelColors :: [Color]
pastelColors = [makeColor 1.0 0.8 0.8 0.9,  -- Pastel Red
                makeColor 1.0 1.0 0.8 0.9,  -- Pastel Yellow
                makeColor 0.8 1.0 0.8 0.9,  -- Pastel Green
                makeColor 0.8 1.0 1.0 0.9,  -- Pastel Cyan
                makeColor 0.8 0.8 1.0 0.9,  -- Pastel Blue
                makeColor 1.0 0.8 1.0 0.9]  -- Pastel Magenta

-- Load the background image
background :: IO Picture
background = do
    maybePic <- loadJuicyPNG "background.png"
    return $ fromMaybe blank maybePic

-- Initial object states with different starting positions, angles, and empty trails
start :: [Object]
start = 
    [ Object (-300, -200) 0.0 10.0 0.0 []    -- First star starting position with an empty trail
    , Object (300, 200) (pi / 2) 15.0 (pi / 4) []  -- Second star with phase shift and an empty trail
    ]

starts :: Simulate
starts = Simulate 0 start False 0.0 0.0 False

-- Loopy path for the first object (moving towards the center)
loopyPath1 :: Float -> (Float, Float)
loopyPath1 time =
    let t = time * 2  -- Speed up the path for visual effect
        radius = 300 * (1 - time / 20)  -- Larger initial radius, moves towards the center
        x = radius * cos t
        y = radius * sin (2 * t)
    in (x, y)

-- Loopy path for the second object (moving towards the center)
loopyPath2 :: Float -> (Float, Float)
loopyPath2 time =
    let radius = 300 * (1 - time / 20)  -- Larger initial radius, moves towards the center
        angle = time * 2  -- Circular motion
        x = radius * cos angle
        y = radius * sin angle
    in (x, y)

-- Update function for animation
update :: ViewPort -> Float -> Simulate -> Simulate
update _ dt sim@(Simulate t objs combined combinedTime growthDuration flicker) =
    if not combined
    then if t < 20  -- For the first 20 seconds, use individual paths
        then Simulate (t + dt) (zipWith updateObject [0..] objs) combined combinedTime growthDuration flicker
        else combineAndGrow sim dt  -- After 20 seconds, combine
    else combineAndGrow sim dt
  where 
    updateObject idx obj@(Object (px, py) angle size phase trail) =
      let newPos = if idx == 0 
                   then loopyPath1 t  -- First object uses loopyPath1
                   else loopyPath2 t  -- Second object uses loopyPath2
          newTrail = take 20 ((px, py) : trail)  -- Update the trail (keep max 20 segments)
      in Object newPos angle size phase newTrail  -- Update position and trail

    combineAndGrow :: Simulate -> Float -> Simulate
    combineAndGrow (Simulate t objs _ combinedTime growthDuration flicker) dt =
        let newGrowthDuration = if t > 20 then growthDuration + dt else 0
            center = (0, 0)

            -- Ensure there are at least two objects to combine
            (obj1, obj2) = case objs of
                [o1, o2] -> (o1, o2)
                _        -> (head objs, head objs)

            (x1, y1) = position obj1
            (x2, y2) = position obj2

            -- Check if the two stars are close enough to combine
            closeEnough = abs (x1 - x2) < 10 && abs (y1 - y2) < 10
            newFlicker = not closeEnough -- Flicker when paths adjust

            -- Phased growth logic: non-linear growth with pauses/shrinks
            combinedSize = if closeEnough
                then let phaseTime = mod' newGrowthDuration 6.0
                    in if phaseTime < 1.0
                            then sum (map size objs) + phaseTime * 10 -- Initial growth
                        else if phaseTime < 2.0
                            then sum (map size objs) - (phaseTime - 1.0) * 2 -- Shrink
                        else if phaseTime < 2.5
                            then sum (map size objs) + (phaseTime - 2.0) * 7 -- Growth again
                        else if phaseTime < 3.5
                            then sum (map size objs) - (phaseTime - 3.0) * 2 -- Shrink again
                        else sum (map size objs) + (phaseTime - 4.0) * 15 -- Final growth
                else sum (map size objs) -- Keep size if not close enough

            -- Adjust positions or create the combined object
            adjustedObjs = if closeEnough
                            then [Object center 0.0 combinedSize 0.0 []]
                            else [ Object (lerp (x1, y1) (x2, y2) 0.05) (angle obj1) (size obj1) (phaseShift obj1) (trail obj1)
                                , Object (lerp (x2, y2) (x1, y1) 0.05) (angle obj2) (size obj2) (phaseShift obj2) (trail obj2)
                                ]
        in Simulate t adjustedObjs closeEnough t newGrowthDuration newFlicker
    -- Update Simulate with adjusted objects

-- Linear interpolation function
lerp :: (Float, Float) -> (Float, Float) -> Float -> (Float, Float)
lerp (x1, y1) (x2, y2) t = (x1 + (x2 - x1) * t, y1 + (y2 - y1) * t)

-- Render function to display the background, objects, and their trails
render :: Picture -> Simulate -> Picture
render bg (Simulate u objs combined _ growthDuration flicker) = Pictures [bg, Pictures (map drawStar objs)]
  where
    drawStar :: Object -> Picture
    drawStar (Object (x, y) _ size _ trail) =
        Pictures [displayCircles size t, trailEffect, flickerEffect, movingStar]
      where
        t = u

        -- Flicker effect
        flickerEffect = if flicker 
                    then color (withAlpha 1.0 white) (rectangleSolid (size * 10) (size * 20))  -- Fill the window with a white flash
                    else Blank

        -- Function to create glowing circles
        -- Function to create glowing star shapes
        glowingStar :: Float -> Float -> Float -> Float -> Color -> Picture
        glowingStar x y size scaleFactor starColor =
            translate x y $ Pictures
                [ outerGlow  -- Outer upright triangle
                , innerGlow  -- Upside-down triangle
                , innerShape  -- Additional inner shape (circle)
                ]
            where
                -- Outer upright triangle
                outerGlow = color (withAlpha 0.7 starColor) $ scale scaleFactor scaleFactor $
                            polygon (triangleVertices size 1)

                -- Upside-down triangle
                innerGlow = color (withAlpha 0.7 starColor) $ scale scaleFactor scaleFactor $
                            polygon (triangleVertices size (-1))

                -- Additional shape inside (circle for a glowing effect)
                innerShape = color (withAlpha 0.9 starColor) $ circleSolid (size * 0.5)

        -- Main display function
        displayCircles :: Float -> Float -> Picture
        displayCircles starSize t = Pictures
            [ glowingStar x y (starSize * 1) 2.0 (pastelColors !! safeIndex1)  -- Glowing star 1
            , glowingStar x y (starSize * 1) 2.5 (pastelColors !! safeIndex2)  -- Glowing star 2
            ]
            where
                index1 = floor (t * 2) `mod` length pastelColors
                safeIndex1 = max 0 (min (length pastelColors - 1) index1)

                index2 = floor ((t * 2 + 3)) `mod` length pastelColors
                safeIndex2 = max 0 (min (length pastelColors - 1) index2)

        -- Star shape with points
        movingStar = translate starX starY $ starShape size
            where
                radius = 100  -- Radius of the circular path
                starX = x   -- x-coordinate of the star
                starY = y -- y-coordinate of the star

            -- Star shape with points (unchanged)
        starShape :: Float -> Picture
        starShape size = Pictures [upwardTriangle, downwardTriangle]
            where
                upwardTriangle = color white $ polygon (triangleVertices size 1)
                downwardTriangle = color white $ polygon (triangleVertices size (-1))

        -- Function to generate vertices for an equilateral triangle
        triangleVertices :: Float -> Float -> [(Float, Float)]
        triangleVertices size direction = 
            [ (0, direction * size)  -- Top vertex
            , (-size * sqrt 3 / 2, -direction * size / 2) -- Bottom-left vertex
            , ( size * sqrt 3 / 2, -direction * size / 2) -- Bottom-right vertex
            ]

        trailEffect = if combined then Blank else Pictures (map drawTrailSegment trail)
        drawTrailSegment (tx, ty) =
            translate tx ty $ color (withAlpha 0.3 yellow) $ circleSolid (size * 0.3)  -- Smaller faded circles

main :: IO ()
main = do
    bg <- background
    simulate
        (InWindow "Loopy Stars with Trails and Background" (800, 600) (10, 10))
        black
        45
        starts
        (render bg)
        update
