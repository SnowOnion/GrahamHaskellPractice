mymap1 f []=[]
mymap1 f xs=[f x|x<-xs]

mymap2 f []=[]
mymap2 f (x:xs)= f x:mymap2 f xs

testmymap1=mymap1 (mymap1 (+1)) [[1, 2, 3], [4, 5]]==[[2,3,4],[5,6]]
testmymap2=mymap2 (mymap2 (+1)) [[1, 2, 3], [4, 5]]==[[2,3,4],[5,6]]

myfilter1 p []=[]
myfilter1 p xs=[x|x<-xs,p x]

myfilter2 p []=[]
myfilter2 p (x:xs)
	|p x = x:myfilter2 p xs
	|otherwise = myfilter2 p xs

testmyfilter1=myfilter1 even [1..5] == [2,4]
testmyfilter2=myfilter2 even [1..5] == [2,4]


-- all :: Foldable t => (a -> Bool) -> t a -> Bool
-- any :: Foldable t => (a -> Bool) -> t a -> Bool


-- takeWhile :: (a -> Bool) -> [a] -> [a]
-- filter :: (a -> Bool) -> [a] -> [a]
-- -- 区别:
-- -- *Main> takeWhile even [2,4,5,7,8,9]
-- -- [2,4]
-- dropWhile :: (a -> Bool) -> [a] -> [a]

length_foldr::Foldable t=>t a->Int -- 为啥不加这句不行呢? 推断不鸟? TODO 
length_foldr=foldr (\_ n->(1+n)) 0

-- reverse_rec :: Foldable t=>t a->t a
reverse_rec :: [a]->[a]
reverse_rec []=[]
reverse_rec (x:xs) = reverse_rec xs ++ [x]

snoc x xs=xs++[x]
reverse_r::[a]->[a]
-- reverse_r::Foldable t=>t a->t a -- 额, 看起来, 如果做这样通用的类型声明, 那么定义就不能写得仅限于[]了!
reverse_r=foldr snoc []

reverse_l::[a]->[a]
reverse_l=foldl (\xs x->x:xs) []

-- 三个reverse都对


type Bit=Int

bin2int_naive :: [Bit]->Int
bin2int_naive bin = sum [x*y|(x,y)<-zip bin weights]
	where
		weights=iterate (*2) 1
testbin2int1=bin2int_naive [1,1,0,1]==11

-- 那啥啥法则 
-- (1∗a)+(2∗b)+(4∗c)+(8∗d) = a +2∗(b +2∗(c +2∗(d +(2∗0))))
bin2int :: [Bit]->Int
bin2int=foldr (\x y->x+2*y) 0
testbin2int2=bin2int [1,1,0,1]==11

int2bin :: Int->[Bit]
int2bin 0=[]
int2bin n = n`mod`2:int2bin (n`div`2)
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

chop8 :: [Bit]->[[Bit]]
chop8 [] =[]
chop8 bits=take 8 bits : chop8 (drop 8 bits)

decode :: [Bit]->String
-- decode xs=chop8 xs|map bin2int|map chr  -- Yin语言之梦 -- 真夏夜の垠梦
-- decode xs=(map chr (map bin2int (chop8 xs))) -- ok
decode=(map chr).(map bin2int).chop8

channel1::[Bit]->[Bit]
channel1=id

channel2=id::[Bit]->[Bit]

channel3=map (\x->1-x) -- 扰动一下! TODO 再说吧.. 随机数啥的还不会

transmit::String->String
transmit=decode.channel2.encode




















