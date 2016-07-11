-- 7-1. Show how the list comprehension [ f x | x ← xs , p x ] can be re-expressed using the higher-order functions map and filter .
compre :: (a->b)->(a->Bool)->[a]->[b]
compre f p xs=map f (filter p xs)

testcompre=compre f p xs==[f x|x<-xs,p x]
	where
		f=(\x->x*x)
		p=even
		xs=[1..5]

-- 7-2. Without looking at the definitions from the standard prelude, define the higher-order functions all, any, takeWhile, and dropWhile.

myall ::  (a -> Bool) -> [a] -> Bool
myall f xs=and (map f xs)
-- myall=and.map -- 为啥不行...(f xs 会被执行? 唔, 二元函数不能用composition?) -- 以及 myall 类型约束写成 Prelude 的 all 的 all :: Foldable t => (a -> Bool) -> t a -> Bool 也会出问题
-- 阿西吧 这个 Foldable... TODO

testmyall1=myall f xs==all f xs
	where
		f=even
		xs=[1..5]

testmyall2=myall f xs==all f xs
	where
		f=even
		xs=[2,4]

testmyall3=myall f xs==all f xs
	where
		f=even
		xs=[]

testmyall=[
	myall even [1..5]==all even [1..5],
	myall even [2,4]==all even [2,4],
	myall even []==all even []]


-- 这个测试框架... 再说吧 TODO

firsti::(a->Bool)->[a]->Int
firsti p xs=length (takeWhile (not.p) xs)

first::(a->Bool)->[a]->a
first p xs=head (dropWhile (not.p) xs)

myany ::  (a -> Bool) -> [a] -> Bool
myany f xs=or (map f xs)
-- myany=or.map

mytakeWhile :: (a -> Bool) -> [a] -> [a]
mytakeWhile p []=[]
mytakeWhile p (x:xs)
	|p x = x:mytakeWhile p xs
	|otherwise = []
mydropWhile :: (a -> Bool) -> [a] -> [a]
mydropWhile p []=[]
mydropWhile p (x:xs)
	|p x = mydropWhile p xs
	|otherwise = x:xs

-- 7.3. Redefine the functions map f and filter p using foldr .
mymap :: (a->b)->[a]->[b]
-- mymap f xs=foldr (\x ys->f x:ys) [] xs -- ok
mymap f=foldr (\x ys->f x:ys) []
testmymap=mymap even [1..5]==map even [1..5]

myfilter :: (a->Bool)->[a]->[a]
-- 好磨人...
myfilter p=foldr (\x ys->if p x then x:ys else ys) []
testmyfilter=myfilter even [1..5]==filter even [1..5]

-- 7.4. Using foldl,define a function dec2int::[Int]→Int that converts a decimal
-- number into an integer. For example:
-- > dec2int [2,3,4,5] 
-- 2345

dec2int::[Int]->Int
-- dec2int xs=foldl (\x y->10*x+y) 0 xs -- ok
dec2int=foldl (\x y->10*x+y) 0
testdec2int=dec2int [2,3,4,5] ==2345

--7.5 Obvious. (ok, the key point is "The types fit"?)
{-
filter even :: Integral a => [a] -> [a]
map (^2) :: Num b => [b] -> [b]
sum :: (Num a, Foldable t) => t a -> a
therefore...
sumsqreven = compose [sum,map (↑2),filter even]
"枪管可以被组装"
-}

-- 7.6 curry 和 uncurry 在 standard prelude 竟然是个函数...

{-
*Main> let f=(\x y->x+y)
*Main> :t f
f :: Num a => a -> a -> a
*Main> let g=(\(x,y)->x+y)
*Main> :t g
g :: Num a => (a, a) -> a
-}

mycurry :: ((a, b) -> c) -> a -> b -> c
myuncurry :: (a -> b -> c) -> (a, b) -> c

mycurry f = (\x y->f (x,y))
myuncurry f = (\(x,y)->f x y)

addCurried=(\x y->x+y)
addOnPairs=(\(x,y)->x+y)

testmycurry=mycurry addOnPairs 1 2==3
testmyuncurry=myuncurry addCurried (1,2)==3

-- 7.7 unfold! interesting

unfold p h t x
	|p x = []
	|otherwise = h x:unfold p h t (t x) -- t 类似 next 函数之类的. head, tail.
-- e.g.
int2bin = unfold (== 0) (`mod`2) (`div`2)
-- solution

type Bit=Int

chop8 :: [Bit]->[[Bit]]
chop8 = unfold null (take 8) (drop 8)

mymap2 :: (a->b)->[a]->[b]
mymap2 f=unfold null (f.head) tail

-- 这个厉害! 发散! 永不止步!
myiterate :: (a->a)->a->[a]
myiterate f=unfold (\_->False) id f

-- 2333333
-- *Main> iterate (*2) 1==myiterate (*2) 1
-- ^CInterrupted.
testmyiterate=take 5(iterate (*2) 1)==take 5(myiterate (*2) 1)

{-
BTW
*Main> take 10 (iterate (*2) 1)
[1,2,4,8,16,32,64,128,256,512]
*Main> take 10 (iterate (*2) 2)
[2,4,8,16,32,64,128,256,512,1024]
*Main> [2^i|i<-[1..10]]
[2,4,8,16,32,64,128,256,512,1024]
*Main> map (2^) [1..10]
[2,4,8,16,32,64,128,256,512,1024]

-}

-- 7.8 parity bits
-- Hint: the library function error :: String → a terminates evaluation and displays the given string as an error message.

-- 那啥啥法则 
-- (1∗a)+(2∗b)+(4∗c)+(8∗d) = a +2∗(b +2∗(c +2∗(d +(2∗0))))
bin2int :: [Bit]->Int
bin2int=foldr (\x y->x+2*y) 0
testbin2int2=bin2int [1,1,0,1]==11

-- int2bin :: Int->[Bit]
-- int2bin 0=[]
-- int2bin n = n`mod`2:int2bin (n`div`2)
testint2bin1=int2bin 11==[1,1,0,1]

make8 :: [Bit]->[Bit]
make8 bits = take 8 (bits ++ repeat 0) -- 惊了! 思路! Holy Laziness!

-- 哎总要copy这个补丁有点烦耶
ord :: Char->Int
ord c=fromEnum c

chr :: Int->Char
chr n=toEnum n::Char

encode :: String ->[Bit]
-- encode xs=foldr (++) [] (map (make8.int2bin.ord) xs)
---- 使用高阶函数concat简化!
-- encode xs=concat (map (make8.int2bin.ord) xs)
---- 老乡, 省参数可以省括号! f(g x)==(f.g) x
encode=concat.map(make8.int2bin.ord)
-- 有区分度的类型. 过了编译就基本错不了((()))

decode :: [Bit]->String
-- decode xs=chop8 xs|map bin2int|map chr  -- Yin语言之梦 -- 真夏夜の垠梦
-- decode xs=(map chr (map bin2int (chop8 xs))) -- ok
decode=(map chr).(map bin2int).chop8

-- structure/record 希望 ( ´_ゝ`)
-- set to one if the number contains an odd number of ones
-- 放在开头, 方便读取...
setParityBit::[Bit]->[Bit]
setParityBit xs
	|length xs==8 = (length (filter odd xs) `mod` 2):xs
	|otherwise = error "setting parity bit for a bit array that length!=8"
testsetParityBit1=setParityBit [1,0,0,0,1,0,0,0]==[0,1,0,0,0,1,0,0,0]
testsetParityBit2=setParityBit [1,0,0,0,1,0,0,1]==[1,1,0,0,0,1,0,0,1]

-- 感谢数字逻辑... 密码学夏亚梅老师- -
checkParityBit::[Bit]->Bool
checkParityBit xs=length (filter odd xs) `mod` 2==0

dropParityBit::[Bit]->[Bit]
dropParityBit xs
	|length xs/=9 = error "dropping parity bit for a bit array that length!=9"
	|not (checkParityBit xs) = error (concat ["failed parity bit checking for ", show xs])
	|otherwise = tail xs

-- make9 :: [Bit]->[Bit]
-- make9 bits = take 9 (bits ++ repeat 0)

encodeWithParityBits::String->[Bit]
encodeWithParityBits=concat.(map setParityBit).map(make8.int2bin.ord)

chop:: Int ->[Bit]->[[Bit]]
chop k=unfold null (take k) (drop k)
chop9 :: [Bit]->[[Bit]]
chop9 = chop 9

decodeWithParityBits::[Bit]->String
decodeWithParityBits=(map chr).(map bin2int).(map dropParityBit).chop9

-- 7.9

transmit::String->String
transmit=decodeWithParityBits.tail.encodeWithParityBits
main=print(transmit "P ZQAE TQR") -- 看不懂的都是表白
-- Exception: failed parity bit checking for [0,0,0,0,0,1,0,0,0]


















