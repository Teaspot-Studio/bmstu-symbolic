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

grnFoo::[Expr]->GenRand Expr
grnFoo (x:xs)  = fromList $ ((x, 1.0 / offsN):) $ map (\a -> (a, toRational $ (fromIntegral $ offsprings a) / offsN) ) xs
    where offsN = fromIntegral $ offsprings x

getRandNode::Expr->GenRand Expr
getRandNode expr@(Val _) = return expr
getRandNode expr@(Var _) = return expr
getRandNode expr@(Sum a b) = grnFoo [expr, a, b]
getRandNode expr@(Minus a b) = grnFoo [expr, a, b]
getRandNode expr@(Mul a b) = grnFoo [expr, a, b]
getRandNode expr@(Div a b) = grnFoo [expr, a, b]
getRandNode expr@(Sin a) = grnFoo [expr, a]
getRandNode expr@(Cos a) = grnFoo [expr, a]
getRandNode expr@(Ln a) = grnFoo [expr, a]
getRandNode expr@(Log a b) = grnFoo [expr, a, b]
getRandNode expr@(Exp a b) = grnFoo [expr, a, b]


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