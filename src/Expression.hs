{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs#-}

module Expression where

import Control.Monad.Random
import Control.Monad.Random.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Applicative

data EvOptions = EvOptions {
                mutationChance::Float,
                elitePart::Float,
                maxGeneration::Int,
                indCount::Int,
                indDepth::Int,
                minVal::Double,
                maxVal::Double,
                varCount::Int
                }

type GenRand = RandT StdGen IO 
type StateGenRand = StateT EvOptions GenRand

data Expr where
    Val     :: Double -> Expr
    Var     :: Int -> Expr
    Sum     :: Expr -> Expr -> Expr
    Minus   :: Expr -> Expr -> Expr
    Mul     :: Expr -> Expr -> Expr
    Div     :: Expr -> Expr -> Expr
    Sin     :: Expr -> Expr
    Cos     :: Expr -> Expr
    Ln      :: Expr -> Expr
    Log     :: Expr -> Expr -> Expr
    Exp     :: Expr -> Expr -> Expr

eval::[Double]->Expr->Double
eval datum (Val x) = x
eval datum (Var n) = datum!!n
eval datum (Sum a b) = eval datum a + eval datum b
eval datum (Minus a b) = eval datum a - eval datum b
eval datum (Mul a b) = eval datum a * eval datum b
eval datum (Div a b) = eval datum a / eval datum b
eval datum (Sin a) = sin(eval datum a)
eval datum (Cos a) = cos(eval datum a)
eval datum (Ln a) = log(eval datum a)
eval datum (Log a b) = logBase (eval datum a) (eval datum b)
eval datum (Exp a b) = eval datum a ** eval datum b


instance Show Expr where
    show (Val x) = show x
    show (Var a) = "X" ++ show a
    show (Sum a b) = "(" ++ show a ++ " + " ++ show b ++ ")"
    show (Minus a b) = "(" ++ show a ++ " - " ++ show b ++ ")"
    show (Mul a b) = "(" ++ show a ++ " * " ++ show b ++ ")"
    show (Div a b) = "(" ++ show a ++ " / " ++ show b ++ ")"
    show (Sin a) = "sin("++ show a ++ ")"
    show (Cos a) = "cos("++ show a ++ ")"
    show (Ln a) = "ln("++ show a ++ ")"
    show (Log a b) = "log_"++ show a ++ "(" ++ show b ++ ")"
    show (Exp a b) = "(" ++ show a ++ "^" ++ show b ++ ")"
    

randTree::StateGenRand Expr
randTree = do
    depth <- indDepth <$> get
    randTree' depth
    where
        getRandVal::StateGenRand Expr
        getRandVal = do
            minV <- minVal <$> get 
            maxV <- maxVal <$> get
            val <- getRandomR (minV, maxV)
            return $ Val val
        getRandVar = do
            n <- varCount <$> get
            x <- uniform [0..n]
            return $ Var x        
        randTree'::Int->StateGenRand Expr
        randTree' depth =
            if(depth == 0)
            then getRandVal
            else do
                expr <- uniform[1..11]
                case expr of 
                    1  -> getRandVal
                    2  -> getRandVar
                    3  -> Sum <$> (randTree' $ depth - 1) <*> (randTree' $ depth - 1) 
                    4  -> Minus <$> (randTree' $ depth - 1) <*> (randTree' $ depth - 1)
                    5  -> Mul <$> (randTree' $ depth - 1) <*> (randTree' $ depth - 1)
                    6  -> Div <$> (randTree' $ depth - 1) <*> (randTree' $ depth - 1)
                    7  -> Sin <$> (randTree' $ depth - 1)
                    8  -> Cos <$> (randTree' $ depth - 1)
                    9  -> Ln  <$> (randTree' $ depth - 1) 
                    10 -> Log <$> (randTree' $ depth - 1) <*> (randTree' $ depth - 1)
                    11 -> Exp <$> (randTree' $ depth - 1) <*> (randTree' $ depth - 1)

grnFoo::Int->[Expr]->GenRand (Expr,Int)
grnFoo d (x:xs) = do
    let offn = fromIntegral $ offsprings x
    index <- fromList $ ((0,toRational $ 1.0/offn):) $ zip [1..] $ map (\a->toRational $(fromIntegral $ offsprings a)/offn) xs
    case index of
        0 -> return (x, d)
        _ -> getRandNode $ ((x:xs)!!index, d+1)

getRandNode::(Expr,Int)->GenRand (Expr,Int)
getRandNode tup@((Val _), d) = return tup
getRandNode tup@((Var _), d) = return tup
getRandNode (expr@(Sum a b), d) = grnFoo d [expr,a,b]
getRandNode (expr@(Minus a b), d) = grnFoo d [expr,a,b]
getRandNode (expr@(Mul a b), d) = grnFoo d [expr,a,b]
getRandNode (expr@(Div a b), d) = grnFoo d [expr,a,b]
getRandNode (expr@(Sin a), d) = grnFoo d [expr,a]
getRandNode (expr@(Cos a), d) = grnFoo d [expr,a]
getRandNode (expr@(Ln a), d) = grnFoo d [expr,a]
getRandNode (expr@(Log a b), d) = grnFoo d [expr,a,b]
getRandNode (expr@(Exp a b), d) = grnFoo d [expr,a,b]

offsprings::Expr->Int
offsprings (Val _) = 1
offsprings (Var _) = 1
offsprings (Sum a b) = 1 + offsprings a + offsprings b 
offsprings (Minus a b) = 1 + offsprings a + offsprings b
offsprings (Mul a b) = 1 + offsprings a + offsprings b
offsprings (Div a b) = 1 + offsprings a + offsprings b
offsprings (Sin a) = 1 + offsprings a 
offsprings (Cos a) = 1 + offsprings a 
offsprings (Ln a) = 1 + offsprings a 
offsprings (Log a b) = 1 + offsprings a + offsprings b
offsprings (Exp a b) = 1 + offsprings a + offsprings b

nodeHeight::Expr->Int
nodeHeight (Val _) = 0
nodeHeight (Var _) = 0
nodeHeight (Sum a b) = 1 + max (nodeHeight a) (nodeHeight b)
nodeHeight (Minus a b) = 1 + max (nodeHeight a) (nodeHeight b)
nodeHeight (Mul a b) = 1 + max (nodeHeight a) (nodeHeight b)
nodeHeight (Div a b) = 1 + max (nodeHeight a) (nodeHeight b)
nodeHeight (Sin a) = 1 + nodeHeight a
nodeHeight (Cos a) = 1 + nodeHeight a
nodeHeight (Ln a) = 1 + nodeHeight a
nodeHeight (Log a b) = 1 + max (nodeHeight a) (nodeHeight b)
nodeHeight (Exp a b) = 1 + max (nodeHeight a) (nodeHeight b)

getNiceNodes::Expr -> Int -> (Int,Int) -> (Int,Int) -> [Expr]
getNiceNodes expr maxDepth orig@(h1,d1) (h2,d2)= acc ++ if (d2 <= maxDepth - h1) && (h2 <= maxDepth - d1) then [expr] else []
    where 
        acc = case expr of
            Val _     -> []
            Var _     -> []
            Sum a b   -> getNiceNodes a maxDepth orig (nodeHeight a, d2+1) ++ getNiceNodes b maxDepth orig (nodeHeight b, d2+1)
            Minus a b -> getNiceNodes a maxDepth orig (nodeHeight a, d2+1) ++ getNiceNodes b maxDepth orig (nodeHeight b, d2+1)
            Mul a b   -> getNiceNodes a maxDepth orig (nodeHeight a, d2+1) ++ getNiceNodes b maxDepth orig (nodeHeight b, d2+1)
            Div a b   -> getNiceNodes a maxDepth orig (nodeHeight a, d2+1) ++ getNiceNodes b maxDepth orig (nodeHeight b, d2+1)
            Sin a     -> getNiceNodes a maxDepth orig (nodeHeight a, d2+1)
            Cos a     -> getNiceNodes a maxDepth orig (nodeHeight a, d2+1)
            Ln a      -> getNiceNodes a maxDepth orig (nodeHeight a, d2+1)
            Log a b   -> getNiceNodes a maxDepth orig (nodeHeight a, d2+1) ++ getNiceNodes b maxDepth orig (nodeHeight b, d2+1)
            Exp a b   -> getNiceNodes a maxDepth orig (nodeHeight a, d2+1) ++ getNiceNodes b maxDepth orig (nodeHeight b, d2+1)
