import Parsing


testparse=parse item "abc"

p :: Parser (Char,Char)
p = do 
	x <- item
	item
	y <- item 
	return (x,y)

testSequence1=parse p "abcdef" -- ==[ (('a', 'c'), "def") ]
testSequence2=parse p "ab" -- ==[]

testChoice1=parse (item +++ return 'd') "abc"
testChoice2=parse (failure +++ return 'd') "ab"

main=do
	print 1
	print 2
	let a=3
	print 4
	print a
	print b
		where b=5

-- parse * 和 +

many :: Parser a->Parser [a]
many p=many1 p +++ return [] -- return [] 并不是失败哦! [] 是"幺元"(零元?)而已...

many1 :: Parser a->Parser [a]
many1 p= do
	v<-p
	vs<-many p
	return (v:vs)

testmany1=parse (many digit) "123abc"==[ ("123", "abc") ]
testmany2=parse (many digit) "abcdef" ==[ ("", "abcdef") ]
testmany3=parse (many1 digit) "abcdef" ==[]

-- 先学会把文法转成这种写法, 写熟了再深入理解原理好了( ﾟ∀。)
natList :: Parser [Int]
natList = do 
	symbol "["
	n <- natural
	ns <- many (do 
		symbol ","
		natural )
	symbol "]"
	return (n : ns) -- 拼装弗兰克斯坦!
-- Note that p only succeeds if a complete list in precisely this format is consumed:
testnatList1=parse natList "[1,2,3,4   , 5, 6   ]   "==[([1,2,3,4,5,6],"")]
testnatList2=parse natList "[1,2,3,4   , 5, 6      "==[]


expr::Parser Int
expr=do 
	t<-term
	do
		symbol "+"
		e<-expr
		return (t+e)
	 	+++ return t -- 我操! 这里为啥是对其之后空一格?? -- 卧槽, 实际上是跟后面的对齐?

term::Parser Int
term=do
	f<-factor
	do
		symbol "*"
		t<-term
		return (f*t)
		+++ return f

factor::Parser Int
factor=do
		symbol "("
		e<-expr
		symbol ")"
		return e
	+++ natural

eval :: String->Int
eval xs=case  parse expr xs of
	[(n,[])]->n
	[(_,out)]->error ("unused input "++out)
	[]->error "invalid input"

-- +++ 的对齐让我很崩溃... 也明白了其实我没明白 sequnce 和 choice...





























