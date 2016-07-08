--5-1
ans1=sum [x^2|x<-[1..100]]
test1=ans1==338350

--5-2
replicate :: Int -> a -> [ a ]
replicate n x=[x|_<-[1..n]]
test2_1=Main.replicate 3 True==[True, True, True ]
test2_2=Main.replicate 0 True==[ ]

--5-3
{-A triple (x, y, z) of positive integers is pythagorean if x^2 + y^2 = z^2.
Using a list comprehension, define a function pyths :: Int → [ (Int , Int , Int ) ] that returns the list of all pythagorean triples whose components are at most a given limit. For example:
> pyths 10
[(3, 4, 5), (4, 3, 5), (6, 8, 10), (8, 6, 10)]
-}

pyths :: Int -> [(Int,Int,Int)]
-- brute force!
pyths n=[(x,y,z)|x<-[1..n],y<-[1..n],z<-[1..n],x^2+y^2==z^2]

-- 用于检查作业 | 我猜华枫写过类似物...
setEqual::Eq a=>[a]->[a]->Bool
-- naive impl (TODO better)
-- another: length==; sort (hmmm... Eq t does not imply Ord t :D) and each==
-- pre contidion: xs1 and xs2 are sets. (elements are distinct)
---- otherwise... setEqual [1,1,2,3] [1,2,3] ; setEqual [1,1,2] [1,2,2]
setEqual xs1 xs2= and ([elem x1 xs2|x1<-xs1]++[elem x2 xs1|x2<-xs2]) -- 这里没括号不行! 函数调用优先于操作符咯?
testSetEqual1=setEqual [1,3,2] [1,2,3]==True
testSetEqual2=setEqual [1,3,2] [1,2,4]==False
testSetEqual3=setEqual [1,3,2] [1,2,3,4]==False
-- 可否证明自反性? 减少一半testcase TODO

testPyths= setEqual (pyths 10) [(3, 4, 5), (4, 3, 5), (6, 8, 10), (8, 6, 10)]

--5-4

factors :: Int->[Int]
factors n=[m|m<-[1..n-1],n`mod`m==0]
testFactors=factors 6==[1,2,3]

perfects :: Int->[Int]
perfects xs=[x|x<-[1..xs],x==sum (factors x)]
testPerfects=perfects 500==[6, 28, 496]

--5-5
--Show how the single comprehension [(x,y) | x ← [1,2,3],y ← [4,5,6]] with two generators 
--can be re-expressed using two comprehensions with single generators
-- 诡异的题目... TODO
tar5=[(x,y) | x <- [1,2,3],y <- [4,5,6]]
-- ([1,2,3],[4,5,6])

-- 5-6

-- 用于本来就key-value pair 的列表...
find :: Eq a => a -> [(a,b)] -> [b] 
find k t = [v|(k',v)<-t,k==k'] -- select v from t where t.k=k
testFind=find 'b'[('a',1),('b',2),('c',3),('b',4)]==[2,4]

positions::Eq a=>a->[a]->[Int]
positions x xs=find x (zip xs [1..])
testPositions=positions 6 [6,8,5,6,7]==[1,4]

-- 5-7
scalarProduct :: [Int]->[Int]->Int
scalarProduct xs ys=sum [x*y|(x,y)<-zip xs ys] 
testScalarProduct=scalarProduct [1, 2, 3] [4, 5, 6]==32

--5-8 Modify the Caesar cipher program to also handle upper-case letters.

--see play.hs
--and the cracking algorithm handles upper-case letters too :)

























