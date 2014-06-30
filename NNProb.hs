
-- Questions sourced from :-> http://www.haskell.org/haskellwiki/99_questions/1_to_10
module NNProb
(myLast
	,myButLast
	,elementAt
	,myLength
	,myReverse
	,isPalindrome
	,myFlatten
	,remove
	,myCompress
	,myPack
	,encode

	) where
	import Data.Char
	import Data.List

	myLast a = last a

	myButLast a = head (tail (reverse a))

	elementAt a b = last (take b a)

	--myLength a = sum (map (+1) (pdt a))
	--pdt a = map (*0) (a)
	myLength :: [a] -> Int -- signature does not mention the type of data in the list
	myLength = sum . map (\_->1) -- good for us

	myReverse :: [a] -> [a]
	myReverse [] = []
	myReverse (a:x) = myReverse(x) ++ [a] 

	isPalindrome a = a == (reverse a)

	data NestedList a = Elem a | List [NestedList a]

	myFlatten :: NestedList a -> [a]
	myFlatten (Elem a) = [a]
	myFlatten (List a) = concatMap myFlatten a


	remove _[] = []
	remove x (a:l) | x == a = remove x l
				   | otherwise = a : remove x l

	myCompress [] = []
	myCompress (a:xs) = a : (myCompress $ dropWhile (==a) xs)

	-- ok, I know this is not what we are looking for but yeah I will work on this later
	-- ****************
	myPack b = group b
	-- ****************

	encode [] = []
	encode xs = [((length a) , (head a)) | a <- group xs]
			


























