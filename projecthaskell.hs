data Rating c= NoRating|R c deriving(Eq,Show)
data Item = I [Char] deriving (Eq,Show)
data User = U [Char] deriving (Eq,Show)

mem :: Eq a => a -> [a]->Bool
mem e []=False
mem e (x:xs)=if(x==e)then True else (mem e xs)

dis :: Eq a => [a] -> [a]
dis [] = []
dis (x:xs)=if((mem x xs) ==True )then (dis xs) else (x:(dis xs))


fromRatingsToItems :: Eq a => [(b,a,c)] -> [a]
fromRatingsToItems [] =[] 
fromRatingsToItems l=dis (items l)

items [] = []
items ((_,x,_):xs)= (x:(items xs)) 

fromRatingsToUsers :: Eq a => [(a,b,c)] -> [a]
fromRatingsToUsers [] = []
fromRatingsToUsers l=dis(users l)

users [] = []
users ((x,_,_):xs)= (x:(users xs)) 

hasRating :: (Eq a, Eq b) => a -> b -> [(a,b,c)] -> Bool
hasRating x y [] =False 
hasRating x y ((z1,z2,_):zs)=if(x==z1 && y==z2)then True else (hasRating x y zs)

getRating :: (Eq a, Eq b) => a -> b -> [(a,b,c)] -> c
getRating x y [] =error"No Given Rating"
getRating x y ((z1,z2,z3):zs)=if(x==z1 && y==z2)then z3 else (getRating x y zs)

formMatrixUser :: (Eq a, Eq b, Fractional c) => b -> [a] -> [(b,a,c)] -> [Rating c]
getRating1 :: (Eq a, Eq b) => a -> b -> [(a,b,c)] -> Rating c
getRating1 x y [] = NoRating
getRating1 x y ((z1,z2,z3):zs)=if(x==z1 && y==z2)then(R z3) else (getRating1 x y zs)
formMatrixUser x [] z = []
formMatrixUser x (y:ys) z=((getRating1 x y z):(formMatrixUser x ys z))



formMatrix :: (Eq a, Eq b, Fractional c) => [b] -> [a] -> [(b,a,c)] -> [[Rating c]]
formMatrix [] y z =[]
formMatrix (x:xs) y z= ((formMatrixUser x y z):(formMatrix xs y z))


numberRatingsGivenItem1 :: (Fractional a, Num b) => Int -> [[Rating a]] -> [b]
index :: [Rating c] -> Int -> Int -> Rating c
index (x:xs) y z =if(y==z)then x else (index xs (y+1) z)

numberRatingsGivenItem1 x [] = [] 
numberRatingsGivenItem1 x (y:ys) = if((index y 0 x) == NoRating) then (0:(numberRatingsGivenItem1 x ys))else 
																((1:numberRatingsGivenItem1 x ys))

numberRatingsGivenItem :: (Fractional a, Num b) => Int -> [[Rating a]] -> b
numberRatingsGivenItem x y =sum(numberRatingsGivenItem1 x y)




differeneRatings :: Fractional a => Rating a -> Rating a -> a
differeneRatings _ NoRating=0.0
differeneRatings NoRating _=0.0
differeneRatings (R x) (R y) = x-y










