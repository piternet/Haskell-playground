module BST where
import Tree

insert :: (Ord a) => a -> Tree a -> Tree a
insert x Empty = Node x Empty Empty
insert x (Node root left right) 
    | x <= root = Node root (insert x left) right
    | otherwise = Node root left (insert x right)

contains :: (Ord a) => a -> Tree a -> Bool
contains x Empty = False
contains x (Node root left right) = 
    x == root || contains x left || contains x right

fromList :: (Ord a) => [a] -> Tree a
fromList l = foldl (\x y -> insert y x) Empty l

toList :: (Ord a) => Tree a -> [a]
toList Empty = []
toList (Node root left right) = (toList left) ++ [root] ++ (toList right)

sortBST :: (Ord a) => [a] -> [a]
sortBST l = toList $ fromList l