module Main where

import Expression
import Control.Monad.Trans.State
import Control.Monad.Random
import Control.Monad

opts = EvOptions {
                mutationChance = 0.1,
                elitePart = 0.1,
                maxGeneration = 10,
                indCount = 10,
                indDepth = 3,
                minVal = 0.5,
                maxVal = 10.0,
                varCount = 3
                }

getIndex::Expr->Int
getIndex (Exp _ _) = 0
getIndex (Sin _)   = 1
getIndex (Val 1)   = 2
getIndex (Sum _ _) = 3
getIndex (Mul _ _) = 4
getIndex (Val 0)   = 5
getIndex (Var 1)   = 6
getIndex (Var 2)   = 7
getIndex _ = -1

check::[Int] -> Expr -> Int -> IO [Int]
check acc expr 0 = return acc
check acc expr n = do
    rng <- newStdGen
    rngExp <- evalRandT (getRandNode expr) rng
    let i = getIndex rngExp
    let newAcc = take i acc ++ [acc!!i + 1] ++ drop (i+1) acc
    check newAcc expr (n-1)

main = do
    let val1 = Exp (Sin (Val 1)) (Sum (Mul (Val 0) (Var 1)) (Var 2))
    let val2 = Mul (Val 1) (Val 0)
    print val1
    print $ offsprings val2
    acc <- check [0,0,0,0,0,0,0,0] val1 1000
    print acc
    {-
    let dat = [7,9,5.467]
    let evalDat = eval dat
    let st = runStateT randTree opts
    rng <- newStdGen
    (expr,_) <- evalRandT st rng
    print expr
    print $ evalDat expr
    -}