module MatrixOp where


boxsize = 3
-- boxs :: [[a]] -> [[a]]
boxs m = (chop boxsize) $ map (chop boxsize) m

chop k [] = []
chop k xs = take k xs : (chop k (drop k xs))

main = print $ boxs (take 9 $ repeat "123456789")