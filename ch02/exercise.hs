-- 2-3

n = a `div` length xs
	where
		a = 10
		xs = [1,2,3,4,5]

-- 2-4

myLast1 xs=head (reverse xs)
myLast2 [x]=x
myLast2 (x:xs)=myLast2 xs

{-
*Main> myLast1 []
*** Exception: Prelude.head: empty list
*Main> myLast2 []
*** Exception: ch02/exercise.hs:(11,1)-(12,25): Non-exhaustive patterns in function myLast2
-}

-- 2-5
myInit1 xs=reverse (tail (reverse xs))
myInit2 [x]=[]
myInit2 (x:xs)=[x]++myInit2 xs