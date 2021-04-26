newton x r  | x*x <= r && (x+1)*(x+1) > r = x
            | otherwise = newton ((x + r `div` x) `div` 2) r
            
raiz n = newton (14*10^(n-1)) (2*10^(2*n))

main = getLine >>= print . raiz . read
