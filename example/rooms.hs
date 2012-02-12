{-# LANGUAGE NoMonomorphismRestriction #-}
-- Ryan Yates
-- 2-12-2012
--
-- Sad program to convert ascii art boxes to diagrams.
--
--     - for horizontal wall
--     | for vertical wall
--     * for a junction
--     , for a downward junction :(
--     > for a rightward junction :(
--
-- The lines still need to be joined up, which means another
-- intermediate layer :(
--
import Control.Arrow((&&&))
import Data.List
import Data.Char
import Diagrams.Prelude
import Diagrams.Backend.Postscript.CmdLine

data Cell = H | V | Join | JoinS | JoinE | Gap
           deriving (Show, Eq)

roomsDef = "*--------  ----*--------------*\n\
           \|              |              |\n\
           \|    Room A         Room B    |\n\
           \|              |              |\n\
           \*---  ----,-  -*----*-----  --*\n\
           \|                   |         |\n\
           \  Room C  | Room D    Room E  |\n\
           \|                   |         |\n\
           \*---------*---------*---------*"

room n w h = text n <> rect w h

toCell '-' = H
toCell '|' = V
toCell '*' = Join
toCell ',' = JoinE
toCell '>' = JoinE
toCell _   = Gap

toHorz V = H
toHorz x = x

onlyHorz V = Gap
onlyHorz x = x

onlyVert H = Gap
onlyVert x = x

lineX l = origin ~~ (origin .+^ (l,0))

mkHLine H     l = lineX  l
mkHLine JoinS l = strutX (l/2) ||| lineX  (l/2)
mkHLine JoinE l = lineX  (l/2) ||| strutX (l/2)
mkHLine _     l = strutX l

map3 f (a:b:bs@(c:_)) = a : map3 f (f a b c : bs)
map3 _ as             = as

map3' g f = reverse . tail . reverse . tail . map3 f . (g:) . (++[g])

rewriteSE Gap Join  x   = JoinS
rewriteSE H   Join  H   = H
rewriteSE H   JoinS H   = H
rewriteSE H   JoinE H   = H
rewriteSE x   Join  Gap = JoinE
rewriteSE _   x    _    = x

setHeight h d = s === d === s
  where s = strutY (h/2)

extractLines (w,h) ls = hs <> vs
  where
    -- extract horizontal and vertical lines as rows
    lsH = map (map (         onlyHorz . toCell)) $ ls
    lsV = map (map (toHorz . onlyVert . toCell))
        . map reverse . transpose $ ls
    
    -- make diagram for row given a width and height for each cell
    f w h = alignTL . setHeight h . hcat . map (g w) . group 
          . map3' Gap rewriteSE
    
    -- make lines from groups
    g l = uncurry mkHLine . (head &&& (*l) . genericLength)
    
    hs = alignTL .                  vcat . map (f w h) $ lsH
    vs = alignTL . rotateBy (1/4) . vcat . map (f h w) $ lsV

mkText (w,h) (x,y) l = strutX ((1/2+x+c)*w) ||| (strutY ((3/4+y)*h) === text l)
  where c = genericLength l / 2
  
extractText c ls = mconcat . map (alignTL . uncurry (mkText c)) . concat
               . map positionY . zip [0..] . map g $ ls
  where
    g = map positionX . f . zip [0..]
    
    positionX [] = (0,[])
    positionX ((x,a):as) = (x, a : map snd as)
    
    positionY (y,as) = map (\(x,ts) -> ((x,y),ts)) as
    
    -- Given a line of text and walls, get the text in groups with
    -- their positions.  Delimit by two consecutive non-alphanumeric.
    f [] = []
    f (a:as) 
      | isAlphaNum (snd a) = (a : reverse as') : f bs'
      | otherwise          = f as
      where
        isT = not . isAlphaNum . snd
        (as',bs') = g [] as
        g rs [] = (rs,[])
        g rs [a]
          | isT a     = (rs,[])
          | otherwise = (a:rs,[])
        g rs (x:y:ys)
          | isT x && isT y = (rs, ys)
          | otherwise      = g (x:rs) (y:ys)


rooms = alignTL r <> alignTL t
  where 
    s = 8
    c = (1,2) ^* s
    ls = lines roomsDef
    r = extractLines c ls # lw (s/2)
    t = extractText c ls


main = defaultMain rooms
