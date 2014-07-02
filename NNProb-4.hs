module NNProb4
(isPrime
,myGcd
,coprime
,totient
,totient'
,primes
,goldbach
,goldbachList
) where
	import Data.List
	
	isPrime a = if length (dropWhile (==0) (tail [ ans | c <- [1..a], let ans = if a `rem` c == 0 then c else 0])) > 2 then False else True
	
	myGcd a b = maximum ( [ ans | c <- [1..a], let ans = if a `rem` c == 0 then c else 0] `intersect` [ ans | c <- [1..b], let ans = if b `rem` c == 0 then c else 0])
	
	coprime a b = if myGcd a b == 1 then True else False
	
	totient a = foldl (+) 0 [ ans | b <- [1..a], let ans = if coprime a b then 1 else 0]
	
	totient' a = length [ b | b <- [1..a], coprime a b] --figured it out late
	
	primes a b = [ c | c <- [a..b], isPrime c ]
	
	goldbach a  
			| a `rem` 2 == 0   = head [(x,y) | x <- (primes 2 a), let y = a-x, isPrime y ]
			| otherwise  = error "Does not hold for odd numbers"

	goldbachList a b = [goldbach c | c <- [a..b], even(c)]