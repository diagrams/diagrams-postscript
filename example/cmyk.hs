import Diagrams.Prelude
import Diagrams.Backend.Postscript.CmdLine
import Diagrams.Backend.Postscript.CMYK

d :: Diagram B R2
d = c ||| m ||| y ||| k
  where
    c = circle 1 # fcCMYK (CMYK 1 0 0 0)
    m = circle 1 # fcCMYK (CMYK 0 1 0 0) 
    y = circle 1 # fcCMYK (CMYK 0 0 1 0) 
    k = circle 1 # fcCMYK (CMYK 0 0 0 1) 

main = mainWith d
