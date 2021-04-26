import Data.List
import Data.Ratio

default (Rational)

-- Esto es para imprimir nada mas

parPrint (x,y) = putStr (show x ++ "\t") >> putStrLn y

main = do
        putStrLn "fABC[x,y,z,w] = (x A y) C (z B w)"
        putStrLn "gABC[x,y,z,w] = ((x A y) B z) C w"
        putStrLn "a R b = b - a"
        putStrLn "a D b = b / a"
        mapM_ parPrint [(map numerator l, f24 l)| 
                        a <- [1..12], 
                        b <- [a..12], 
                        c <- [b..12], 
                        d <- [c..12],
                        let l = map toRational [a,b,c,d],p24 l
                    ]

-- El codigo en serio es esta parte.

f [a,b,c,d] o1 o2 o3 = ((a `o1` b) `o3` (c `o2` d)) == 24
g [a,b,c,d] o1 o2 o3 = (((a `o1` b) `o2` c) `o3` d) == 24

midiv _ 0 = 0
midiv a b = a / b

lop = [(+),(-), flip (-),(*),midiv, flip midiv]

p24 l = or [any (\x -> f x o1 o2 o3 || g x o1 o2 o3) . nub . permutations $ l | 
                o1 <- lop, 
                o2 <- lop,
                o3 <- lop]
                     
opString op = case numerator $ op 12 4 of
                     16 -> "+"
                     8  -> "-"
                     -8 -> "R"
                     48 -> "*"
                     3  -> "/"
                     1  -> "D" 
                     
s24 l = solu f "f" ++ solu g "g"
            where solu f sf = [ sf ++ concatMap opString [o1,o2,o3] ++ show (map numerator p) | 
                                    o1 <- lop, 
                                    o2 <- lop,
                                    o3 <- lop,
                                    p <- nub $ permutations l,
                                    f p o1 o2 o3]
                

f24 = head . s24

c24 = length . s24
