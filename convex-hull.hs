import Text.Printf
import Data.Function (on)
import Data.List (sortBy, sort)
import Data.Tuple (swap)

solve :: [(Int, Int)] -> Double
solve points = perimeter $ 
               (\(p:ps) -> gscan p ps) (lowest_y d_points) 
    where d_points = sort $
                     map (\(x,y) -> (fromIntegral x, fromIntegral y)) points


min_perimeter :: (Double,Double) -> (Double,Double) -> (Double,Double) -> [(Double,Double)] -> Double
min_perimeter start prev current []     = dist current start
min_perimeter start prev current points = (\x -> (\(a,b,c) -> a
										  	+ min_perimeter start current (b,c) (
										  		map (\(q,r,s)-> (r,s)) (tail x))
										  	) (head x)
                                          ) $ min_dist current points


--go by sorted x values and split points into above and below
walk_perimeter  p q points = walk_perimeter' p ((filter (\x -> isAbove x p q) points) ++ [q])
                           + walk_perimeter' p ((filter (\x -> isBelow x p q) points) ++ [q])
walk_perimeter' current []     = 0
walk_perimeter' current points = dist current (head points)  
                               + walk_perimeter' (head points) (tail points)


perimeter :: [(Double,Double)] -> Double
perimeter points | length points > 0 = perimeter' (points ++ [head points])
	             | otherwise         = 0.0

perimeter' [] = 0	                           
perimeter' (p:ps) | length ps == 0 = 0.0
	              | otherwise      = (dist p (head ps)) + perimeter' ps


max_x points = last points
min_x points = head points


min_dist :: (Double,Double) -> [(Double,Double)] -> [(Double,Double,Double)]
min_dist (p_x,p_y) points = sortBy (compare `on` (\(a,b,c)-> a)) $
                            	map (\(x,y) -> ((dist (x,y) (p_x,p_y)),
                                      	             x,
                                      	             y)) points


max_dist :: [(Double,Double)] -> (Double,Double) -> (Double,Double) -> (Double,Double,Double)
max_dist points (p_x,p_y) (q_x,q_y) = head $ sortBy (compare `on` (\(a,b,c)-> (-a))) $
                                      map (\(x,y) -> ((line_dist (x,y) (p_x,p_y) (q_x,q_y)),
                                      	             x,
                                      	             y)) points


dist :: (Double, Double) -> (Double, Double) -> Double
dist (a_x,a_y) (b_x,b_y) = sqrt $ (b_x-a_x)^2 + (b_y-a_y)^2

line_dist :: (Double, Double) -> (Double, Double) -> (Double, Double) -> Double
line_dist (x,y) (p_x,p_y) (q_x,q_y) = abs ( (q_y-p_y)*x - (q_x-p_x)*y + q_x*p_y - q_y*p_x ) / 
	                                  sqrt ( (q_y-p_y)^2 + (q_x-p_x)^2 )


--find furthest point & split up remaining into 2 lines from the furthest point 
find_hull []     p q = []
find_hull points p q = 
	(\(a,b,c) -> [(b,c)] ++ 
        find_hull (filter (\(x,y) -> x<b && not (isInside (x,y) p q (b,c))) points) p     (b,c) ++
        find_hull (filter (\(x,y) -> x>b && not (isInside (x,y) p q (b,c))) points) (b,c) q) 
	$ max_dist points p q


--if triangle a b c then p is inside if all cross products points in same direction
isInside p a b c = 0 <= v && v <= 1 &&
				   0 <= w && w <= 1 &&
				        v + w  <= 1 where 
	v0    = transform b a
	v1    = transform c a
	v2    = transform p a
	d00   = dot v0 v0
	d01   = dot v0 v1
	d11   = dot v1 v1
	d20   = dot v2 v0
	d21   = dot v2 v1
	denom = d00*d11 - d01*d01
	v     = (d11*d20 - d01*d21)/denom
	w     = (d00*d21 - d01*d20)/denom
	u     = 1.0 - v - w
	dot (x,y) (a,b) = x*a + y*b
	transform (x,y) (a,b) = (x-a,y-b)


--filter hull into above and below line
init_split_hull points p q = [p] ++ 
	                         (find_hull (filter (\x -> isAbove x p q) points) p q) ++
	                         [q] ++ 
                              find_hull (filter (\x -> isBelow x p q) points) p q

isAbove (x,y) (p_x,p_y) (q_x,q_y) = y > m*x + (p_y-m*p_x)  &&
                                    (x /= q_x && y /= q_y  &&
                                     x /= p_x && y /= p_y)
	where m = (p_y-q_y)/(p_x-q_x)

isBelow (x,y) (p_x,p_y) (q_x,q_y) = y < m*x + (p_y-m*p_x)  &&
                                    (x /= q_x && y /= q_y  &&
                                     x /= p_x && y /= p_y)
	where m = (p_y-q_y)/(p_x-q_x)

main :: IO ()
main = do
  n <- readLn :: IO Int
  content <- getContents
  let  
    points = map (\[x, y] -> (x, y)). map (map (read::String->Int)). map words. lines $ content
    ans = solve points
  printf "%.1f\n" ans


lowest_y :: (Ord a, Num a, Ord b, Num b) => [(a,b)] -> [(a,b)]
lowest_y points = map swap $ sort $ (map swap) points


--3 points are counter-clockwise turn if ccw > 0, clockwise if ccw < 0 and colinear if = 0
ccw :: (Double, Double) -> (Double, Double) -> (Double, Double) -> Double
ccw (ax,ay) (bx,by) (cx,cy) = (bx-ax)*(cy-ay) - (by-ay)*(cx-ax)


angle_sort :: (Double, Double) -> [(Double, Double)] -> [(Double, Double)]
angle_sort p points = sortBy (compare `on` angle p) points where
	angle (px,py) (ax,ay) = (atan2 (ay-py) (ax-px), abs(ax-px))


gscan :: (Double, Double) -> [(Double, Double)] -> [(Double, Double)]
gscan p points 
    | length points >= 3 = scan [p] (angle_sort p points)
    | otherwise          = [p] ++ points 


scan :: [(Double, Double)] -> [(Double, Double)] -> [(Double, Double)]
scan [p] (p1:ps) 
   | ccw p p1 (last ps) == 0 = [(last ps), p]
scan (x:xs) (y:z:rsts)  
   | ccw x y z < 0   = scan xs       (x:z:rsts)
   | ccw x y z == 0  = scan (x:xs)   (z:rsts)
   | ccw x y z > 0   = scan (y:x:xs) (z:rsts)
scan xs [z] = z : xs