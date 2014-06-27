work k = doubleMe k + k
doubleMe k = k*2
replacer k = [ if x `mod` 2 == 1 then "Odd" else "lol" | x <- k, x /= 15] -- all but 15
rightTriangles x y z = [ (a,b,c) | a <- x, b <- y, c <- z, a^2 + b^2 == c^2 ]
scaleneTriangles p q r = [ (a,b,c) | a <- p, b <- q, c <- r, a == b && b /= c ]
fibonacci p q r = [ (a,b,c) | a <- p, b <- q, c <- r, a+b == c ]
plucky :: (Integral a) => a -> String  
plucky 7 = "LUCKY NUMBER SEVEN!"  
plucky x = "Sorry, you're out of luck, pal!" 

adding :: (Num a) => (a,a) -> (a,a) -> (a,a)
adding (x,y) (p,q) = (x+p, y+q)

maxi' :: (Ord a) => [a] -> a
maxi' [] = error "Empty"
maxi' [j] = j
maxi' (j:xs) = max j (maxi' xs)

apply2 :: (a -> a) -> a-> a
apply2 f x = f (f x)

map' :: (a -> b) => [a] -> [b]
map' _[] = []
map' f (x:xs) = f x : map' f xs

lardiv :: (Integral a) => a
lardiv = head (filter p [1000,999..])
	where p x = x `mod` 69 == 0

cylinder :: (RealFloat a) => a -> a -> a  
cylinder r h = 
    let sideArea = 2 * pi * r * h  
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea 
