{-# LANGUAGE NoMonomorphismRestriction #-}
-- Addapted from: "The MathematicalOrchid, 24 Feb 2007"
--  http://warp.povusers.org/MandScripts/haskell.html

import Data.Complex
import Diagrams.Prelude hiding (magnitude,image)
import Diagrams.Backend.Postscript.CmdLine

quadratic c z = z*z + c

critical_orbit z = iterate (quadratic z) 0

pixel = length . takeWhile (\z -> magnitude z <= 2) . take max_iter

side n v0 v1 =
   let sv = (v1 - v0) / fromIntegral n
   in  [v0, (v0 + sv) .. v1]

grid = map (\y -> map (:+ y) side_x) side_y

image = map (map (to_circle . pixel . critical_orbit)) grid

d :: Diagram Postscript R2
d = vcat . map hcat $ image

main = defaultMain d

side_x = side 64 (-2) 2
side_y = side 64 (-2) 2

max_iter = 32

minBox = centerXY (strutX w <> strutY w)
  where w = 2 * sqrt max_iter

to_circle :: Integral a => a -> Diagram Postscript R2
to_circle = (<> minBox) . centerXY . lw 0 . fc blue . circle . sqrt . fromIntegral