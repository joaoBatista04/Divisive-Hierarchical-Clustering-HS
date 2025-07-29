module UnionFind (UnionFind, initUF, find, union) where

--The structure stores the parents of each point
--Obs.: This version of Union-Find isn't the most optimized, because further optimizations would need assessments in variables (which would make the code very complex and difficult to read and would violate the principle of the functional paradigm)
data UnionFind = UnionFind { parent :: [Int] }

--Function to create the initial UnionFind structure
initUF :: [Int] -> UnionFind
initUF ids = UnionFind { parent = ids }

--Function to find the representative (root) of a point
find :: UnionFind -> Int -> Int
find uf x = if parent uf !! (x - 1) == x 
            then x 
            else find uf (parent uf !! (x - 1))

--Function to join two sets (points) recursively
union :: UnionFind -> Int -> Int -> UnionFind
union uf x y = 
    if rootX == rootY then uf
    else UnionFind { parent = unionAux (parent uf) x rootY }
  where
    rootX = find uf x
    rootY = find uf y

--Recursive helper function to update parent point
unionAux :: [Int] -> Int -> Int -> [Int]
unionAux [] _ _ = []
unionAux (p:ps) idx newRoot
    --Replaces the first index with the new root
    | idx == 1    = newRoot : ps
    --Continues on the list
    | otherwise   = p : unionAux ps (idx - 1) newRoot