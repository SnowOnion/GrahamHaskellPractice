-- 为了能test, 加了 Eq a 的约束... 啊 这个不可判等的集合元素
-- 不! 不需要加 Eq a 也能编译...
(+++) :: [a]->[a]->[a]
[]+++ys=ys
(x:xs)+++ys=x:(xs+++ys)

-- testCat1=[]==([]+++[]) 

-- 我擦! 就testCat1不过编译 TODO

-- ch06/play.hs:6:12:
--     No instance for (Eq t0) arising from a use of ‘==’
--     The type variable ‘t0’ is ambiguous
--     Note: there are several potential instances:
--       instance (Eq a, Eq b) => Eq (Either a b)
--         -- Defined in ‘Data.Either’
--       instance forall (k :: BOX) (s :: k). Eq (Data.Proxy.Proxy s)
--         -- Defined in ‘Data.Proxy’
--       instance (GHC.Arr.Ix i, Eq e) => Eq (GHC.Arr.Array i e)
--         -- Defined in ‘GHC.Arr’
--       ...plus 28 others
--     In the expression: [] == ([] +++ [])
--     In an equation for ‘testCat1’: testCat1 = [] == ([] +++ [])
-- Failed, modules loaded: none.


testCat2=[1,2]+++[]==[1,2]
testCat3=[]+++[3,4]==[3,4]
testCat4=[1,2]+++[3,4]==[1,2,3,4]

-- testCat= and [testCat1 testCat2 testCat3 testCat4]
testCat0=[1,2]==[3,4]

-- 保序插入
insert :: Ord a=>a->[a]->[a]
insert x []=[x]
insert x (y:ys)
	|y>=x = x:y:ys
	|otherwise = y:insert x ys

-- 插入排序 这就很自然... "递归信任"->"n确实值得信任"->去把后面排好
isort :: Ord a=>[a]->[a]
isort []=[]
isort (x:xs)=insert x (isort xs)
-- 我怎样觉悟到要加这个括号: 第一坨报错的最后一句是
-- In an equation for ‘isort’: isort (x : xs) = insert x isort xs

-- 又会 "The type variable ‘a0’ is ambiguous"
-- testIsort1=isort []==[] 
testIsort2=isort [6,1,2,4,3]==[1,2,3,4,6]
testIsort3=isort ['a']==['a']

myzip :: [a]->[b]->[(a,b)]
myzip [] _ =[]
myzip _ [] =[]
myzip (x:xs) (y:ys) = (x,y):myzip xs ys

testMyzip=myzip [1,2] [3,4,5]==[(1,3),(2,4)]

mydrop :: Int->[a]->[a]
mydrop n xs | n<=0 = xs
-- mydrop (n+1) (x:xs)=mydrop n xs -- Integer pattern is removed from Haskell! 
mydrop n []=[]
mydrop n (x:xs) = mydrop (n-1) xs -- n==0已经被匹配掉了!
testMydrop1=mydrop 0 [2,3,4]==[2,3,4]
testMydrop2=mydrop 2 [2,3,4]==[4]
testMydrop3=mydrop 4 [2,3,4]==[]
-- testMydrop4=mydrop 2 []==[]














