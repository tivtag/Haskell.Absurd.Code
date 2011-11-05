--
-- A silly console fractal printer.
--
-- Usage: putFract mandelbrot
--

module Fractal where
    import Data.Complex
    
    type FractalFunction = (Complex Float -> Complex Float -> Complex Float)    
    iterationDepth = 500

    calculatePoint :: Complex Float -> FractalFunction -> Complex Float
    calculatePoint a f = iterate (f a) a !! iterationDepth

    insideSet :: Complex Float -> FractalFunction -> Bool
    insideSet point f = 
        mag < 2 
        where mag = magnitude $ calculatePoint point f
        
    generateFractal :: FractalFunction -> [String]
    generateFractal f = 
        [[if insideSet (x :+ y) f then '*' else ' ' | x <- [-2, -1.9685 .. 0.5]] | y <- [1, 0.95 .. -1]]
        
    putFract :: FractalFunction -> IO ()
    putFract f = mapM_ putStrLn $ generateFractal f
        
    --
    -- Example Functions:
    --
     
    mandelbrot :: FractalFunction
    mandelbrot = \a z -> z^2 + a
        
    julia3 :: FractalFunction
    julia3 = \_ z -> z^3 - (0.5 :+ 0.1)