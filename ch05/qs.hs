qs []=[]
qs (x:xs)=qs smaller ++ [x] ++ qs larger
	where {smaller=[y|y<-xs,y<=x];
		larger=[y|y<-xs,y>x]}
test=qs [1,5,2,3,4]

