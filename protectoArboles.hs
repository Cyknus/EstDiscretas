data BinTreeInt = Empty | Root Int BinTreeInt BinTreeInt deriving Show

preorder :: BinTreeInt -> [Int]
preorder Empty = []
preorder (Root n t1 t2) = [n] ++ preorder t1 ++ preorder t2

inorder :: BinTreeInt -> [Int]
inorder Empty = []
inorder (Root n t1 t2) = inorder t1 ++ [n] ++ inorder t2

postorder :: BinTreeInt -> [Int] 
postorder Empty = []
postorder (Root n t1 t2) = postorder t1 ++ postorder t2 ++ [n]

reflect :: BinTreeInt -> BinTreeInt
reflect Empty =  Empty
reflect (Root n t1 t2) = Root n (reflect t2) (reflect t1)

height :: BinTreeInt -> Int
height Empty = -1
height (Root n Empty Empty) = 0
height (Root n t1 t2) = 1 + max (height t1) (height t2)

size :: BinTreeInt -> Int
size Empty = 0
size (Root n t1 t2) = 1 + size t1 + size t2

leafs :: BinTreeInt -> Int
leafs Empty = 0
leafs (Root n Empty Empty) = 1
leafs (Root n t1 t2) = leafs t1 + leafs t2

insertBST :: Int -> BinTreeInt -> BinTreeInt
insertBST n Empty = Root n Empty Empty
insertBST n (Root m t1 t2) = if n>m then
							Root m t1 (insertBST n t2) else
							Root m (insertBST n t1) t2	

crea_arbol_busqueda :: [Int] -> BinTreeInt
crea_arbol_busqueda [] = Empty
crea_arbol_busqueda as = insertBST (last as) $ crea_arbol_busqueda $ init as

busca :: Int -> BinTreeInt -> Bool
busca n Empty = False
busca n (Root m t1 t2)
	| n == m = True
	| n > m  = busca n t2
	| otherwise = busca n t1
	
test
