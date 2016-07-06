--4-1
halve :: [ a ] ->([ a ], [ a ]) 
halve xs=
	if even n
	then (take (n`div`2) xs, drop (n`div`2) xs)
	else ([],[]) --一开始写的是(). 类型检查发现了这个错误! 甜头+1
	-- 自作主张了... 当然需求完备是最好的.. "需求不完备"时Haskell惯例是啥? 抛出匹配失败? 返回错误码? TODO 
	where n=length xs

--4-2
--(a)
safetail1 :: [ a ] -> [ a ]
safetail2 :: [ a ] -> [ a ]
safetail3 :: [ a ] -> [ a ]

safetail1 xs=
	if null xs
	then xs
	else tail xs

safetail2 xs
	| null xs = xs
	| otherwise =tail xs

safetail3 []=[]
safetail3 (x:xs)=xs

--4-3
-- 本来想叫 &&1 但是不行 TODO operator允许的命名方式
(&&&) :: Bool->Bool->Bool
True &&& True=True
True &&& False=False
False &&& True=False
False &&& False=False

(&&&&) :: Bool->Bool->Bool
True &&&& True=True
_ &&&& _=False

(&&&&&) :: Bool->Bool->Bool
True &&&&& a=a
False &&&&& _=False

(&&&&&&) :: Bool->Bool->Bool
a &&&&&& b
	|a==b =a
	|otherwise =False


(|||) :: Bool->Bool->Bool
True ||| True=True
True ||| False=True
False ||| True=True
False ||| False=False

(||||) :: Bool->Bool->Bool
False |||| False=False
_ |||| _=True

(|||||) :: Bool->Bool->Bool
False ||||| a=a
True ||||| _=True

(||||||) :: Bool->Bool->Bool
a |||||| b
	|a==b =a
	|otherwise =True

--需要自动化测试 ( ´_ゝ`)

-- 单个用例对答案, f a b == model a b
same ::(Bool->Bool->Bool)->(Bool->Bool->Bool)->(Bool,Bool)->Bool
same f model (a,b)=f a b==model a b
-- ok::
ok f model []=True
ok f model (x:xs)= (same f model x) && (ok f model xs) -- 括号不加不行... && 优先级好高啊... operator 高于普通函数? TODO
-- ok f model=ok f model [(True,True),(True,False),(False,True),(False,False)] 
--不能搞这种参数数量不同的overloading...
-- Equations for ‘ok’ have different numbers of arguments
allcorrect f model=ok f model [(True,True),(True,False),(False,True),(False,False)] 

andList=[(&&&),(&&&&),(&&&&&),(&&&&&&)]
orList=[(|||),(||||),(|||||),(||||||)]
testand=map (\x->allcorrect x (&&)) andList
testor=map (\x->allcorrect x (||)) orList

-- 非常好, 都一遍过 ( ﾟ∀。)

--4-4
and4 p q=
	if p
	then
		if q
		then True
		else False
	else False
test4=allcorrect and4 (&&)
--4-5
and5 p q=
	if p
	then q
	else False
test5=allcorrect and5 (&&)

--4-6
-- multxx :: b->(b->(b->b)) --哎?
-- 哇塞! 贴心建议... 但是这背后... TODO
--     No instance for (Num b) arising from a use of ‘*’
--     Possible fix:
--       add (Num b) to the context of
--         the type signature for multxx :: b -> b -> b -> b
multxx :: Num b=> b -> b -> b -> b
-- 注意括号
multxx=(\x->(\y->(\z->x*y*z)))
















