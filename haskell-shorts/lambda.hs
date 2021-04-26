import Data.List
import Data.Maybe

type Variable = String
data LambdaExpression = App LambdaExpression LambdaExpression | Abs Variable LambdaExpression | Var Variable

-- Expression printing

instance Show LambdaExpression where 
    showsPrec _ (Var s) = (s++)
    showsPrec 0 (App f e) = showsPrec 0 f . (" " ++) . showsPrec 1 e
    showsPrec p (App f e) = ("(" ++) . showsPrec 0 f . (" " ++) . showsPrec 1 e . (")" ++)
    showsPrec _ (Abs x e) = (("(\\" ++ x ++ ".") ++) . showsPrec 0 e . (")"++)

-- Expression parsing

hasAny s l = or $ map (`elem` l) s

validVarName x = not $ null x || hasAny "()\\. " x

varRead s = [(Var variable, rest) | (variable, rest) <- lex s, validVarName variable]

readMany r s = concat [ ([token], r0):map (\(l, r1) -> (token:l, r1)) (readMany r r0) | (token, r0) <- r s]
appRead 0 s = [(foldl1 App lf, r0) | (lf, r0)  <- readMany (readsPrec 1) s,
                               length (take 2 lf) == 2]
appRead p s = [(e, r2) | ("(",r0) <- lex s,
                         (e, r1)  <- appRead 0 r0,
                         (")",r2) <- lex r1]


instance Read LambdaExpression where 
    readsPrec p s  = varRead s ++ -- VarParse
                       [(Abs x e, r5) | ("(", r0)  <- lex s,
                             ("\\", r1) <- lex r0,
                             (Var x,r2) <- varRead r1,
                             (".",r3)   <- lex r2,
                             (e, r4)    <- readsPrec 0 r3,
                             (")",r5)   <- lex r4] ++ -- AbsParse
                       appRead p s -- AppParse

-- Expression substitution-related stuff

freshVariables = map (('x':) . show) [1..]

firstFresh e = head $ dropWhile (`isFree` e) freshVariables

isFree x (Var y)   = x == y
isFree x (App f e) = isFree x f || isFree x e
isFree x (Abs y e) | x == y    = False
                   | otherwise = isFree x e

replace :: Variable -> LambdaExpression -> LambdaExpression -> LambdaExpression
replace y e2 (Var x) | x == y    = e2
                     | otherwise = Var x
replace y e2 (App f e) = App (replace y e2 f) (replace y e2 e)
replace y e2 (Abs x e) | x == y      = Abs x e
                       | isFree x e2 = Abs z (replace y e2 (replace x (Var z) e))
                       | otherwise   = Abs x (replace y e2 e)
                            where z = firstFresh e2


-- Single step beta reduction, normal evaluation order (lazy)
eval :: LambdaExpression -> Maybe LambdaExpression
eval (Var x  ) = Nothing
eval (App (Abs x e) e2) = Just $ replace x e2 e
eval (App f e) = fmap (\f -> App f e) (eval f)
eval (Abs x e) = Nothing

-- Full evaluation until normal form is reached (infinite list if there is no normal form)
fullEval = map fromJust . takeWhile isJust . iterate (eval . fromJust) . Just

-- Input / Output , user interaction

maxReductionSteps = 100
firstTermsToShow = 8
lastTermsToShow = 8

prettyPrint allReductions | length reductions == maxReductionSteps = 
                                       ("Error! Too many reductions (" ++ show maxReductionSteps ++ ") performed. Computation aborted."):
                                            take firstTermsToShow reductions ++ ".....": take lastTermsToShow reductions
                          | otherwise = reductions
                                where reductions = map show (take maxReductionSteps allReductions)

main = interact $ concatMap (unlines . map ("  "++) . prettyPrint . fullEval . read) . takeWhile (/="exit") . lines
