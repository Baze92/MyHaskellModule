-- Questions sourced from :-> http://www.haskell.org/haskellwiki/99_questions/21_to_28
module NNProb3
(insertAt
	) where

		insertAt ch li ind = (take (ind-1) li) ++ (ch ++ (drop (ind - 1) li))
