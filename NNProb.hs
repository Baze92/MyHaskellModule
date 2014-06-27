-- Questions sourced from :-> http://www.haskell.org/haskellwiki/99_questions/1_to_10
module NNProb
(myLast
	,myButLast
	,elementAt
	,myLength
	) where

	myLast a = last a

	myButLast a = head (tail (reverse a))

	elementAt a b = last (take b a)

	myLength a = sum (map a