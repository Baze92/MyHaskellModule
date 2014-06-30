-- Questions sourced from :-> http://www.haskell.org/haskellwiki/99_questions/11_to_20
module NNProb2
(encodeModified
	,decodeModified
	,dupli
	,repli
	,dropEvery
	,split
	,slice
	,rotate
	,removeAt
	)
	where
	import Data.List

	data NonHomo a = Single a | Multiple Int a
		deriving (Show)
	encodeModified [] = []
	encodeModified xs = [ans | x <- group xs, let ans = if (length x) == 1 then Single (head x) else Multiple (length x) (head x)]

	decodeModified :: [NonHomo a] -> [a] 
	decodeModified = concatMap deco
		where
		deco (Single a)	    = [a]
		deco (Multiple a b) = replicate a b

	dupli [] = []
	dupli xs = concat [replicate 2 a | a<-xs]

	repli []_ = []
	repli xs b = concat [replicate b a | a<-xs]

	dropEvery xs a
 	 	 | length xs < a = xs
 		 | otherwise     = take (a-1) xs ++ dropEvery (drop a xs) a

 	split xs a = splitAt a xs

 	slice xs a b = fst (splitAt (b-2) (snd (splitAt (a-1) xs)))

 	rotate xs a 
 		| a > 0  = snd (splitAt (a) xs) ++ fst (splitAt (a) xs)
 		| a < 0  = snd (splitAt ((length xs)+a) xs) ++ fst (splitAt ((length xs)+a) xs)
 		| otherwise = xs

 	removeAt a xs = (xs !! (a-1), take (a-1) xs ++ drop a xs)

















