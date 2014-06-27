module Math
(replacer
    ,rightTriangles
    ,scaleneTriangles
    ,vectorAdd2D
    ,vectorAdd3D
    ,maxInList
    ,largestMultiple42
    ,factorial
    ,series2
    ,seriesGeneric
    ,sumFirstN
    ,sumFirstNSquares
    ,sumFirstGeneric
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

    factorial :: (Integral a) => a -> a
    factorial 0 = 1
    factorial a = a * factorial (a - 1)

    series2 a = [ 2^k |  k <- a]

    seriesGeneric a b = [ b^k |  k <- a ]
    

    sumFirstN :: (Integral n) => n -> n
    sumFirstN n = sum [1..n]

    sumFirstNSquares :: (Integral n) => n -> n
    sumFirstNSquares 0 = 0
    sumFirstNSquares n =
        let a = n
        in  (a^2) + sumFirstNSquares (a-1)


    sumFirstGeneric :: (Integral n) => n -> n -> n
    sumFirstGeneric 0 q = 0
    sumFirstGeneric p q =
        let a = p
            pow = a ^ q
        in  pow + sumFirstGeneric (a-1) q    
