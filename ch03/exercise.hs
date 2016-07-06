{-
3-1
[Char]
(Char,Char,Char)
[(Bool,Char)]
([Bool],[Char])
[[a]->[a]]

对啦!

3-2
[a]->a
(a,a)->(a,a)
GHCi说 *Main> :t swap
swap :: (t1, t) -> (t, t1)
有道理... 他俩不一定同类型...

a->a->(a,a)
Num a=>a->a
Eq a=>[a]->Bool
想: 能apply两次, 肯定(?)是个变换(ノﾟ∀ﾟ)ノ所以f的type是a->a...
(a->a)->a->a
	yes! GHCi 也推断(?)出这个结果!
		它的推断方式和我一样吗? (´・ω・`)

*Main> :t once
once :: (t1 -> t) -> t1 -> t
一颗赛艇...


3-3
好 我用GHCi检查..


-}

x11=['a','b','c']
x12=('a', 'b', 'c')
x13=[ (False , 'O'), (True , '1') ] 
x14=([False,True],['0','1'])
x15=[ tail , init , reverse ]

second xs=head (tail xs)
swap (x,y)=(y,x)
pair x y=(x,y)
double x=x*2
palindrome xs=reverse xs == xs
twice f x=f(f x)

once f x=f x