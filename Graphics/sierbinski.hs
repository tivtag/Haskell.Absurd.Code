--
-- Displays an animated "Sierpinski Triangle".
--
-- Requires the "Gloss" library to be installed: http://gloss.ouroborus.net/
-- Requires GLUT to run. Win32 download: http://user.xmission.com/~nate/glut.html
--

import Graphics.Gloss
import Graphics.Gloss.Data.Point
  
--
-- Pure Code
--

-- Represents a triangle with the Points A, B and C
type Triangle = (Point, Point, Point)

-- Creates the smaller triangle inside the given triangle.
middleTriangle :: Triangle -> Triangle
middleTriangle ((ax, ay), (bx, by), (cx, cy)) =
      (((ax + bx) / 2.0, (ay + by) / 2.0), 
       ((ax + cx) / 2.0, (ay + cy) / 2.0),
       ((bx + cx) / 2.0, (by + cy) / 2.0))
 
-- Splits the given triangle into one larger triangle in the middle and
-- the three smaller triangles on the sides.
splitTriangle :: Triangle -> (Triangle, Triangle, Triangle, Triangle)
splitTriangle tri@(a, b, c) = (middle, (a, ab, ac), (ab, b, bc), (ac, bc, c))
    where middle@(ab, ac, bc) = middleTriangle tri
          
-- Returns the sierpinski triangles fitting n-levels deep into the given triangle.
sierpinski :: Integer -> Triangle -> [Triangle]
sierpinski 0 _ = []
sierpinski n tri = middle : (sierpinski nextN t1) ++ (sierpinski nextN t2) ++ (sierpinski nextN t3)
    where (middle, t1, t2, t3) = splitTriangle tri
          nextN                = n - 1
  
-- Returns the iteration time for the given frame time. Used for animation.
timeToIteration frameTime = (round $ frameTime * animSpeed) `mod` maxIteration
    where maxIteration = 10
          animSpeed    = 2.0
  
  
--  
-- Drawing Code
--

-- Converts the given colored triangle into a picture.
drawTriangle :: Color -> Triangle -> Picture
drawTriangle color (a, b, c)
    = Color color
    $ Polygon [a, b, c]

-- Combines the given same-colored triangles into a single picture.
drawTriangles :: Color -> [Triangle] -> Picture
drawTriangles color tris 
    = Color color
    $ Pictures $ map (drawTriangle color) tris

-- Draws the sierpinski triangle n-iterations deep; starting at the given triangle.
drawSierpinski n tri = Pictures [drawTriangle white tri, drawTriangles black tris]
    where tris = sierpinski n tri

-- Draws a single frame of the sierpinski animation. 
-- The shown iteration-depth is based on the current frameTime.
sierpinskiFrame frameTime	
    = Translate (-250) (-150)
    $ Scale 5.0 5.0
    $ drawSierpinski (timeToIteration frameTime) ((0,0), (100, 0), (50,75))

-- Creates, shows and runs the windows and the animation loop.
main = animateInWindow
    "Sierpinski Triangle" -- window title
    (800, 600)            -- window size
    (10, 10)              -- window position
    black                 -- background color
    sierpinskiFrame       -- picture to display based on frameTime
        