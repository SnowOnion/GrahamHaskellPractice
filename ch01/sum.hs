mySum []=0
mySum(x:xs)=x+mySum xs

qsort []=[]
qsort (x:xs)=
	qsort smaller ++ [x] ++ qsort larger -- 取第一个做 pivot!
	where -- where 搞 local definition
		smaller=[a|a<-xs,a<=x]
		larger=[a|a<-xs,a>x]
		-- 语法设定这个 <- 是因为长得像 ∈ (\in) 么 ( ´_ゝ`)

-- main = print (qsort [3,3,1,3,2,4,5])

-- ex 1-3 pdf22
myProduct []=1
myProduct (x:xs)=x*myProduct xs

qsortRev []=[]
qsortRev (x:xs)=
	qsortRev larger ++ [x] ++ qsortRev smaller
	where
		smaller=[a|a<-xs,a<=x]
		larger=[a|a<-xs,a>x]

main = print (qsortRev [3,3,1,3,2,4,5])

