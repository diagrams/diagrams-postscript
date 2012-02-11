import Diagrams.Prelude
import Diagrams.Backend.Postscript.CmdLine
import Data.List.Split

number d n = alignT (text t <> square 1 # lw 0) ||| alignT d
  where t = concat ["(", show n, ")"]
        
grid r ds = ds' # chunk r # map hcat # vcat
  where ds' = map (alignTL . (<> (square m # lw 0 # alignTL))) ds
        m   = maximum (map width ds <> map height ds)
        
polyLines l = b ~~ d <> d ~~ a
  where
    n = 5
    [a,b,c,d,e] = polyVertices with
           { polyType = PolySides
                        (repeat (1/ fromIntegral n :: CircleFrac))
                        (replicate (n-1) l)
           , polyOrient = OrientH
           }

circuits = zipWith number cs' [1..]
         # grid 3
         # centerXY # pad 1.1
  where
    v = (1/2,1/2)
    s = square 1
    l = (origin .-^ v) ~~ (origin .+^ v)
    c = s <> l
    t = eqTriangle 1
    t' = scaleY 0.5 $ eqTriangle 1
    tl = alignT t <> origin ~~ (origin .-^ (1/2,0))
    x = s <> l <> reflectY l
    cs = [ pad 1.5 x 
         , t' === x
         , t === reflectY t
         , rotateBy (1/4) $ t' === reflectY (alignB t' <> alignB t)
         , alignBR s === alignTL s
         , alignBR (reflectY c) === alignTL c
         , pentagon 1 <> polyLines 1
         , (alignB t ||| alignT (reflectY tl))
         ]
    cs' = map (pad 1.1 . centerXY . scale 80) cs

main = defaultMain circuits