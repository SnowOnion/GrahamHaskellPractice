--6-1
(^-^) :: Integer->Integer->Integer
b^-^0=1
b^-^n=b*(b^-^(n-1))

test11=3^-^0==1
test12=4^-^3==64

-- Int [-2^63, 2^63-1]

-- 必须要有快速幂w
(^--^) :: Integer->Integer->Integer
b^--^0=1
b^--^1=b
b^--^2=b*b
b^--^n
	|n`mod`2==0 = (b^--^(n `div` 2))^--^2
	|otherwise = b^--^half * b^--^remain
		where
			half=n `div` 2
			remain=n-half

test13=2^--^1000000==2^1000000
--true

--6-2 too simple!

--6-3 too simple...
-- – Decide if all logical values in a list are True: and :: [Bool]→Bool
-- – Concatenate a list of lists: concat :: [[a]]→[a]
-- – Produce a list with n identical elements: replicate :: Int→a→[a]
-- – Select the nth element of a list: (!!) :: [a]→Int→a
-- – Decide if a value is an element of a list: elem :: Eqa⇒a→[a]→Bool

--6-4 do not use function "insert". use explicit recursion.
merge :: Ord a=>[a]->[a]->[a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) 
	| x<y = x:merge xs (y:ys)
	|otherwise = y:merge (x:xs) ys

testmerge1 = merge [2,5,6] [1,3,4]==[1..6]
testmerge2 = merge [] [1,3,4]==[1,3,4]
testmerge3 = merge [1,3,4] []==[1,3,4]
testmerge4 = merge [2,5,6] [1,3]==[1,2,3,5,6]
testmerge5 = merge [2,5] [1,3,4]==[1..5]
-- testmerge4 = merge [] []==[]

--6-5
halve :: Ord a => [a]->([a],[a])
halve []=([],[])
halve [x]=([],[x])
halve xs=(take half xs, drop half xs)
	where
		half= (length xs) `div` 2

mergesort :: Ord a=> [a]->[a]
mergesort []=[]
mergesort [x]=[x]
-- 模式匹配定义函数... 必须放到一坨?! TODO 否则:
-- ch06/exercises.hs:64:1:
--     Multiple declarations of ‘mergesort’
--     Declared at: ch06/exercises.hs:52:1
--                  ch06/exercises.hs:64:1
mergesort xs=merge (mergesort left) (mergesort right)
	where 
		(left,right)=halve xs

testmergesort1 = mergesort [2,5,6,1,3,4]==[1..6]
testmergesort2 = mergesort [2,5,1,3,4]==[1..5]
-- 真的是... 不知道怎么就做对了orz 
-- 当然 之前就做过这一套也是很大的

---------
--这个 计算机科学的实验呀
--野心, 是到这之后才有的. 之前, 没做过这么大的梦.
---------

--6-6
mysum::Num a=>[a]->a
mysum []=0
mysum (x:xs)=x+mysum xs

-- not difficult 
























