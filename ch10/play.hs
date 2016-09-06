safediv::Int->Int->Maybe Int
safediv _ 0=Nothing
safediv m n=Just (m`div`n)

-- ok, 产生了把数从 Just 里拿出来的需求...

data Tree = Leaf Int | Node Tree Int Tree
t1 = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

occurs :: Int -> Tree -> Bool 
occurs m (Leaf n) = m==n
occurs m (Node l n r) = m==n || occurs m l || occurs m r

-- 中序遍历咯?
flatten :: Tree->[Int]
flatten (Leaf n) = [n]
flatten (Node l n r)=flatten l ++ [n] ++ flatten r
-- If applying this function results in a sorted list, then the tree itself is called a search tree. 
-- 所以 occurs 变二分

occurs m (Leaf n)= m==n
occurs (Node l n r)
	|m==n = True
	|m<n = occurs m l
	|otherwise = occurs m r


