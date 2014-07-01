module NNProb4
(isPrime
	,myGcd
	,coprime
	) where
		import Data.List

		isPrime a = if length (dropWhile (==0) (tail [ ans | c <- [1..a], let ans = if a `rem` c == 0 then c else 0])) > 2 then False else True

		myGcd a b = maximum ( [ ans | c <- [1..a], let ans = if a `rem` c == 0 then c else 0] `intersect` [ ans | c <- [1..b], let ans = if b `rem` c == 0 then c else 0])

		coprime a b = if myGcd a b == 1 then True else False