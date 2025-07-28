module UnionFind (UnionFind, initUF, find, union) where

-- Definição da estrutura Union-Find
data UnionFind = UnionFind { parent :: [Int] }  -- Armazena os pais de cada ponto

-- Função para criar a estrutura UnionFind inicial
initUF :: [Int] -> UnionFind
initUF ids = UnionFind { parent = ids }

-- Função para encontrar o representante (root) de um ponto
find :: UnionFind -> Int -> Int
find uf x = if parent uf !! (x - 1) == x 
            then x 
            else find uf (parent uf !! (x - 1))

-- Função para unir dois conjuntos (pontos) de forma recursiva sem usar let
union :: UnionFind -> Int -> Int -> UnionFind
union uf x y = 
    if rootX == rootY then uf
    else UnionFind { parent = unionAux (parent uf) x rootY }
  where
    rootX = find uf x
    rootY = find uf y

-- Função auxiliar recursiva para atualizar o pai
unionAux :: [Int] -> Int -> Int -> [Int]
unionAux [] _ _ = []  -- Caso base: lista vazia
unionAux (p:ps) idx newRoot
    | idx == 1    = newRoot : ps   -- Substitui o primeiro índice pelo novo root
    | otherwise   = p : unionAux ps (idx - 1) newRoot  -- Continua na lista