-- Questions sourced from :-> http://www.haskell.org/haskellwiki/99_questions/11_to_20
module NNProb2
(encodeModified
	,decodeModified
	,dupli
	,repli
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