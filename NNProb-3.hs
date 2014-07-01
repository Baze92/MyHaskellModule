-- Questions sourced from :-> http://www.haskell.org/haskellwiki/99_questions/21_to_28
module NNProb3
(insertAt
	,range
	,rndSelect
	) where
		import System.Random

		insertAt ch xs ind = (take (ind-1) xs) ++ (ch ++ (drop (ind - 1) xs))

		range a b = [ c | c <- [a..b]]

		-- ok I need to read up on the use of Random functions. 
		-- Keeping these on the back-burner
		rndSelect xs a = fst(randomRIO ())