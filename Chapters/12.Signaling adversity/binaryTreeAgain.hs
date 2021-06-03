module BinaryTreeAgain where

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)


unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f x = go f x (f x)
 where go _ x Nothing = Leaf
       go f _ (Just (x, y, z)) = Node (unfold f x) y (unfold f z)

treeBuild :: Integer -> BinaryTree Integer
treeBuild x = unfold (\x -> if x == 0 then Nothing else Just ((x-1),(x-1),(x-1))) x