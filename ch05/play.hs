list1=[[3*x,3*x+1,3*x+2]|x<-[0..4]]
list2=[(3*x,3*x+1,3*x+2)|x<-[0..4]]

firsts1 xs=[x|[x,_,_]<-xs] 
firsts1' xs=[head x|x<-xs]
firsts2 xs=[x|(x,_,_)<-xs]

-- *Main> :t firsts1
-- firsts1 :: [[t]] -> [t]
-- *Main> :t firsts2
-- firsts2 :: [(t, t1, t2)] -> [t]
-- 棒棒的, 既允许用户写类型, 又能推断类型...

myLength xs = sum[1|_<-xs]
-- 不像python... Haskell 里 length (1,2,3) 是不可以的.

positions::Eq a=>a->[a]->[Int]
positions x xs=[i|(i,x')<-zip [0..] xs,x==x']
list3=[6,8,5,6,7]
testPositions=positions 6 list3==[1,4]

--To convert a Char to or from the corresponding Int value defined by Unicode, use toEnum and fromEnum from the Enum class respectively (or equivalently ord and chr).
--https://hackage.haskell.org/package/base-4.9.0.0/docs/Prelude.html
-- TODO 哎为啥书里直接就用 ord 了呢?
{-
toEnum :: Enum a => Int -> a
fromEnum :: Enum a => a -> Int
-}
ord :: Char->Int
ord c=fromEnum c

chr :: Int->Char
chr n=toEnum n::Char

let2int_lower :: Char -> Int
let2int_lower c = (ord c) - (ord 'a')

let2int_upper :: Char -> Int
let2int_upper c = (ord c) - (ord 'A')

int2let_lower :: Int->Char
int2let_lower n = chr (n+ord 'a')

int2let_upper :: Int->Char
int2let_upper n = chr (n+ord 'A')

isLower::Char->Bool
isLower c=ord 'a'<=ord c && ord c <= ord 'z'

isUpper::Char->Bool
isUpper c=ord 'A'<=ord c && ord c <= ord 'Z'

lowers :: [Char]->Int
lowers xs=length [x|x<-xs,isLower x]
testLowers=lowers ['A','b','c','D','e']==3

uppers :: [Char]->Int
uppers xs=length [x|x<-xs,isUpper x]
testUppers=uppers ['A','b','c','D','e']==2

count :: Eq a=>a->[a]->Int
count x xs=length [x'|x'<-xs,x==x']
testCount=count 'b' ['A','b','b','D','b']==3

shift :: Int->Char->Char
shift n c
	| isLower c = int2let_lower((let2int_lower c + n)`mod`26)
	| isUpper c = int2let_upper((let2int_upper c + n)`mod`26)
	| otherwise = c

encode :: Int->String->String
encode n xs=[shift n x|x<-xs]
testEncode1=encode 3 "haskell is fun"=="kdvnhoo lv ixq"
testEncode2=encode 3 "Haskell is Fun WXYZ"=="Kdvnhoo lv Ixq ZABC"

decode n xs=encode (-n) xs
testDecode1=decode 3 "Kdvnhoo lv Ixq ZABC"=="Haskell is Fun WXYZ"


-- Crack!

table :: [ Float ]
table = [ 8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4,6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1 ]

percent :: Int -> Int -> Float
-- percent :: -> Float -> Float
-- GHCi no fromInt ... why?
percent n m = ( fromIntegral n /fromIntegral  m) * 100 :: Float

freqs :: String->[Float]
-- freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
-- 	where n = lowers xs
-- 大小写都适用的版本
freqs xs = [percent (count l xs + count u xs) n | (l,u) <- zip ['a'..'z'] ['A'..'Z']]
	where n = lowers xs + uppers xs

--方差~
chisqr :: [Float ] -> [Float ] -> Float
chisqr os es = sum[((o-e)^2)/e|(o,e)<-zip os es]

-- 向左"轮换". 小轮组合美如画...
rotate :: Int->[a]->[a] 
rotate n xs = drop n xs++take n xs
-- 啊.. TDD...
testRotate0 = rotate 3 [1,2,3,4,5]==[4,5,1,2,3]

crack :: String -> String 
crack xs = decode factor xs
	where
		factor = head (positions (minimum chitab) chitab) 
		chitab = [chisqr (rotate n table') table | n <- [0 .. 25]] 
		table' = freqs xs

testCrack1=
	crack "kdvnhoo lv ixq"
	=="haskell is fun"
testCrack2=
	crack "vscd mywzboroxcsyxc kbo ecopev"
	=="list comprehensions are useful"
testCrack3=
	crack "Kdvnhoo lv Ixq ZABC"
	=="Haskell is Fun WXYZ" -- 请包容乱码.. -- 实测不包容2333
testCrack4=
	crack "Kdvnhoo lv Ixq LHHH"
	=="Haskell is Fun IEEE" -- 包容成功
-- amazing!


















