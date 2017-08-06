module Matrix where


{--
 - TODO : Matrix can be monad 
 - instance (Monad a) => Monad (Mat a) where ...
 -
 --}

data Mat a = Mat [[a]]

showRow [] = ""
showRow (x:xs) = show x ++ "\t\b" ++ showRow xs
instance (Show a) => Show (Mat a) where 
    show (Mat []) = ""
    show (Mat (x:xs)) = showRow x ++ "\n" ++ show (Mat xs) 

len_row x = maximum $ map length x
len_col x = length x
instance (Num a) => Num (Mat a) where
    (Mat a) + (Mat b) =
        let m = max(len_row a)(len_row b) in
        let n = max(len_col a)(len_col b) in
        Mat [
                [ (a!!i!!j) + (b!!i!!j) | j <- [0..(n-1)]] 
                | i <- [0..(m-1)]
            ]
    (Mat a) - (Mat b) =
        let m = max(len_row a)(len_row b) in
        let n = max(len_col a)(len_col b) in
        Mat [
                [ (a!!i!!j) - (b!!i!!j) | j <- [0..(n-1)]] 
                | i <- [0..(m-1)]
            ]
    (Mat a) * (Mat b) =
        let l = len_row a - 1 in
        let m = len_col a - 1 in
        let n = len_col b - 1 in
        Mat [
                [ sum [((a!!i!!k)*(b!!k!!j)) | k<-[0..m]] | j<-[0..n] ] 
                | i<-[0..l] 
            ]


