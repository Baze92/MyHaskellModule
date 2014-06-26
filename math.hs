module Math
(replacer
	,rightTriangles
	,scaleneTriangles
	,vectorAdd2D
	,vectorAdd3D
	,maxInList
	,largestMultiple42
	) where

	replacer k = [ if x `mod` 3 == 1 then "3Div" else "nay" | x <- k, x /= 15]

	rightTriangles x y z = [ (a,b,c) | a <- x, b <- y, c <- z, a^2 + b^2 == c^2 ]

	scaleneTriangles p q r = [ (a,b,c) | a <- p, b <- q, c <- r, a == b && b /= c ]

	vectorAdd2D :: (Num a) => (a,a) -> (a,a) -> (a,a)
	vectorAdd2D (x,y) (p,q) = (x+p, y+q)

	vectorAdd3D :: (Num a) => (a,a,a) -> (a,a,a) -> (a,a,a)
	vectorAdd3D (x,y,z) (p,q,r) = (x+p, y+q, z+r)

	maxInList :: (Ord a) => [a] -> a
	maxInList [] = error "Empty list"
	maxInList [z] = z
	maxInList (z:zs) = max z (maxInList zs)

	largestMultiple42 :: (Integral a) => a
	largestMultiple42 = head (filter p [10000,9999..])
		where p x = x `mod` 42 == 0