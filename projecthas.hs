-- datatypes
data Item = I [Char]   deriving(Show,Eq)
data User = U [Char]   deriving(Show,Eq)
data Rating c =NoRating|R c deriving(Show,Eq)

-- takes a list of values and returns a list of distinct values.
dis :: Eq a => [a] -> [a]
dis [] =[]
dis (x:xs) | (mem x xs) = dis xs
           | otherwise = (x:(dis xs))


mem :: Eq a => a -> [a] -> Bool
mem a [] =False
mem a (x:xs) | a==x =True
             | otherwise = mem a xs
			 
-- takes a list of ratings of users to items and returns the list of items.
fromRatingsToItems :: Eq a => [(b,a,c)] -> [a] 
fromRatingsToItems x =y where y= dis f where f = helper1 x

helper1 [] =[]
helper1 ((_,x,_):xs) = (x:(helper1(xs)))

-- takes a list of ratings of users to items and returns
fromRatingsToUsers :: Eq a => [(a,b,c)] -> [a] 
fromRatingsToUsers x =y where y= dis f where f = helper2 x

helper2 [] =[]
helper2 ((x,_,_):xs) = (x:(helper2(xs)))

-- takes a user, item and a list of ratings. It returns True if the user rated this item and False otherwise.
hasRating :: (Eq a, Eq b) => a -> b -> [(a,b,c)] -> Bool
hasRating _ _ []=False
hasRating x y ((t,f,_):xs) | ((x==t)&&(y==f))=True
                        | otherwise =(hasRating x y xs)
						
--takes a user, item and a list of ratings.It returns the rating given by this user to this item.						
getRating :: (Eq a, Eq b) => a -> b -> [(a,b,c)] -> c
getRating _ _ [] = error "No given rating"
getRating x y ((t,f,c):xs) | ((x==t)&&(y==f))=c
                        | otherwise =(getRating x y xs)	
					
--takesa user, list of available items and a list of ratings. It returns a list with the rating given by this user to
--each item. The rating is given through the type Rating as shown in the below example.					
formMatrixUser :: (Eq a, Eq b, Fractional c) => b -> [a] -> [(b,a,c)] -> [Rating c] 
formMatrixUser _ [] _=[]
formMatrixUser a (c:d) z = (if (hasRating a c z)then (R w) else NoRating):(formMatrixUser a d z) where w = (getRating a c z)
           
--takes the list of available users, the list of available items and a list of ratings. It returns a matrix of the rating
--given by each user to each item using the typr Rating		   
formMatrix :: (Eq a, Eq b, Fractional c) => [b] -> [a] -> [(b,a,c)] -> [[Rating c]]		   
formMatrix [] _ _=[]
formMatrix (a:b) z d =(formMatrixUser a z d): (formMatrix b z d)

--takes the index of an item and the matrix of ratings and returns the number of ratings given to this item
numberRatingsGivenItem :: (Fractional a, Num b) => Int -> [[Rating a]] -> b
numberRatingsGivenItem _ [] =0
numberRatingsGivenItem a (x:xs)| ((index a 0 x) /= NoRating)=1+(numberRatingsGivenItem a xs)
                               | otherwise = (numberRatingsGivenItem a xs)


index a z (x:xs) = if(z==a)then x else (index a (z+1) xs) 

--takes 2 ratings and returns the difference between them.
differeneRatings :: Fractional a => Rating a -> Rating a -> a
differeneRatings NoRating _ =0
differeneRatings _ NoRating=0
differeneRatings (R x) (R a) = x-a

--takes a number and constructs a lis of pairs osall the numbers between 0 and the input number -1.
matrixPairs :: (Num a,Ord a) => a -> [(a,a)]
matrixPairs a = twolist 0 a

list b s a = if(s > a)then [] else ((b,s)):(list b (s+1) a)

twolist s a = if(s > a)then [] else (list s 0 a) ++ (twolist (s+1) a)

--takes the ratings’ matrix and returns a matrix for the items ratings differences. The value at row i and column j represents the sum of
--the differences between the ratings given to item i and item j by the same user.
dMatrix :: Fractional a => [[Rating a]] -> [a]
dMatrix (a:ass) = (list2 s (a:ass))where s= matrixPairs ((length a)-1)

list1 c []=0
list1 c (a:ass)= (differeneRatings (index i 0 a) (index j 0 a))+(list1 c ass) where (i,j)=c

list2 [] a =[]
list2 (x:xs) a = (list1 x a):(list2 xs a)

--takes the ratings’ matrix and returns a matrix for the frequency of items’ ratings. The value at row i and column j represents
--number of users who rated both item i and j.
freqMatrix :: (Num a,Ord a, Fractional b) => [[Rating b]] -> [a]
freqMatrix (a:ass) = (list4 s (a:ass))where s= matrixPairs ((length a)-1)

list3 c (a:ass)= getsma (numberRatingsGivenItem i (a:ass)) (numberRatingsGivenItem j (a:ass)) where (i,j)=c

list4 [] a =[]
list4 (x:xs) a = (list3 x a):(list4 xs a)

getsma a b | a <b =a
           | otherwise =b
		   
--takes the ratings’ matrix and returns a matrix for the average differences (using the frequencies).
diffFreqMatrix :: (Ord a,Fractional a) => [[Rating a]] -> [a] 
diffFreqMatrix a = list6 x a where x=dMatrix a

list6 x a = list5 x y where y=freqMatrix a

list5 [] []=[]
list5 (x:xs) (y:ys)=(x/y):(list5 xs ys)

--The function predict is used to predict the rating a user would have given an item. It takes the ratings
--matrix and the index of the user and the index of the item whose rating is missing. The prediction is
--used as the average prediction based on all the differences relevant to this item.
predict a iu it = helper12 c s a iu it where (c,s)=((fromRatingsToUsers a),(fromRatingsToItems a))
helper12 c s a iu it =if(hasRating (index iu 0 c) (index it 0 s) a)then w else (helper13 c s a iu it) where (R w)= (index it 0 (index iu 0 (formMatrix c s a)))
helper13 c s a iu it = helper (length s) d iu it where d=(formMatrix c s a)
helper  f a iu it = helper0 0 0 0 c z (it*f) where (c,z)=((diffFreqMatrix a),(index iu 0 a))
helper0 x y _ _ [] _= (y/x)
helper0 x y d z (a:ass) b |(a==NoRating)=(helper0 x y (d+1) z ass b )  
       					| otherwise = (helper0 (x+1) (w+s+y) (d+1) z ass b) where (R w,s)=(a,(index (b+d) 0 z)) 
