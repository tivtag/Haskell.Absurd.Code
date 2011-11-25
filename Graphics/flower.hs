--
-- Displays an animated "Seed of Life".
--
-- Requires the "Gloss" library to be installed: http://gloss.ouroborus.net/
-- Requires GLUT to run. Win32 download: http://user.xmission.com/~nate/glut.html
--

import Graphics.Gloss
import Graphics.Gloss.Data.Point

--
-- Pure Code
--

addPoint :: Point -> Point -> Point
addPoint (x,y) (x2, y2) = (x+x2, y+y2)
                      
pointFromAngle :: Float -> Float -> Point
pointFromAngle length angle = (length * cos angle, length * sin angle)

circlePolygon :: Point -> Float -> Float -> Float -> Float -> Path
circlePolygon center radius startAngle stepSize maxAngle = 
    let transform = (addPoint center) . (pointFromAngle radius) . (+startAngle)
        angels    = [0,stepSize..maxAngle] in
    map transform angels
        
getSeedPositions :: Float -> Float -> Float -> [Point]
getSeedPositions radius stepSize maxAngle =    
    center : (circlePolygon center radius 0 stepSize maxAngle)
    where center = (0, 0)
        
--
-- Draw Logic
--

drawCircle :: Float -> Point -> Picture
drawCircle radius (x,y)
    = Translate x y
    $ Color black
    $ Circle radius

drawCircles :: [Point] -> Float -> Picture
drawCircles positions radius
    = Rotate (radius * 6)
    $ Pictures (map (drawCircle radius) positions )
    
seedOfLifeFrame :: Float -> Picture
seedOfLifeFrame frameTime
    = Pictures[drawCircles positions radius, drawCircle (2*radius) (0,0)]
    where radius   = 4 * frameTime
          stepSize = pi / 3
          maxAngle = 2.0*pi
          positions = getSeedPositions radius stepSize maxAngle

-- Creates, shows and runs the windows and the animation loop.
main = animateInWindow
    "Life"                -- window title
    (800, 600)            -- window size
    (10, 10)              -- window position
    white                 -- background color
    seedOfLifeFrame       -- picture to display based on frameTime