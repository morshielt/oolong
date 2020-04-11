-- {-# LANGUAGE FlexibleContexts #-}
module Interpreter where

import           AbsOolong

import           Types

import           Data.Map                      as M
import           Data.Maybe

import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Except

returnNothing :: IMon (VarToLoc, ReturnResult)
returnNothing = do
    env <- ask
    return (env, Nothing)

evalExprM :: Expr -> IMon ReturnResult
evalExprM (EString s) = (return . Just . VString) s
evalExprM (ELitInt n) = (return . Just . VInt) n
evalExprM ELitTrue    = (return . Just . VBool) True
evalExprM ELitFalse   = (return . Just . VBool) False
evalExprM (Neg e)     = do
    e' <- evalExprM e
    case e' of
        (Just e'') -> case e'' of
            (VInt n) -> (return . Just . VInt . negate) n
            _        -> error "TODO Inappropriate type for integer negation"
        Nothing -> error "TODO Nothing to integer negate"

evalExprM (Not e) = do
    e' <- evalExprM e
    case e' of
        (Just e'') -> case e'' of
            (VBool b) -> (return . Just . VBool . not) b
            _         -> error "TODO Inappropriate type for bool negation"
        Nothing -> error "TODO Nothing to bool negate"


evalExprM (EMul e symbol f) = do
    e' <- evalExprM e
    f' <- evalExprM f
    return $ liftM2 (performMulOp symbol) e' f'

evalExprM (EAdd e symbol f) = do
    e' <- evalExprM e
    f' <- evalExprM f
    return $ liftM2 (performAddOp symbol) e' f'

evalExprM (ERel e symbol f) = do
    e' <- evalExprM e
    f' <- evalExprM f
    return $ liftM2 (performRelOp symbol) e' f'

performMulOp :: MulOp -> Val -> Val -> Val
performMulOp op (VInt a) (VInt b) = VInt $ mulOp op a b
performMulOp _  _        _        = error "performMulOp"

mulOp :: MulOp -> Integer -> Integer -> Integer
mulOp Times = (*)
mulOp Div   = div
mulOp Mod   = mod

performAddOp :: AddOp -> Val -> Val -> Val
performAddOp op (VInt a) (VInt b) = VInt $ addOp op a b
performAddOp _  _        _        = error "performAddOp"

addOp :: AddOp -> Integer -> Integer -> Integer
addOp Plus  = (+)
addOp Minus = (-)

performRelOp :: RelOp -> Val -> Val -> Val
performRelOp op (VInt a) (VInt b) = VBool $ relOp op a b
performRelOp _  _        _        = error "performRelOp"

relOp :: RelOp -> Integer -> Integer -> Bool
relOp LTH = (<)
relOp LE  = (<=)
relOp GTH = (>)
relOp GE  = (>=)
relOp EQU = (==)
relOp NE  = (/=)

-- performArithmOp :: ArithmOp o => o -> Val -> Val -> Val
-- performArithmOp o (VInt a) (VInt b) = VInt $ relOp o a b
-- performArithmOp _ _        _        = error "TODO Invalid performArithmOp"

-- performRelationOp :: RelationOp o => o -> Val -> Val -> Val
-- performRelationOp o (VInt a) (VInt b) = VBool $ relOp o a b
-- performRelationOp _ _        _        = error "TODO Invalid performRelationOp"


-- performArithmOp :: Op -> Val -> Val -> Val
-- performArithmOp relOp a b = undefined
-- performArithmOp _ _ _ = error "TODO Invalid operation"


execStmtM :: Stmt -> IMon (VarToLoc, ReturnResult)
execStmtM Empty      = returnNothing

execStmtM (SPrint e) = do
    e' <- evalExprM e
    liftIO $ putStrLn $ valToString e'
    returnNothing

execStmtM _ = undefined


valToString :: ReturnResult -> String
valToString res = case res of
    (Just val) -> case val of
        (VInt    x) -> show x
        (VBool   b) -> show b
        (VString s) -> s
        _           -> error "ERROR TODO"
    Nothing -> error "ERROR TODO"
-- valToString (VInt    x) = show x
-- valToString (VBool   b) = show b
-- valToString (VString s) = s
-- valToString _           = error "ERROR TODO"


-- valToString (VInt    x) = show x
-- valToString (VBool   b) = show b
-- valToString (VString s) = s
-- valToString _           = "ERROR TODO"

    -- do
    -- env <- ask
    -- return (env, e')

-- execStmtM (Decl e) = do
--     env <- ask
--     let e' = evalExprM e env
--     modify (Map.insert x e')


interpretMany :: [Stmt] -> IMon (VarToLoc, ReturnResult)
interpretMany (s : xs) = do
    (env, ret) <- execStmtM s
    if isNothing ret
        then local (const env) (interpretMany xs)
        else return (env, ret)
interpretMany [] = returnNothing

runInterpreter prog =
    runExceptT $ runStateT (runReaderT (interpretMany prog) M.empty) $ IMState
        M.empty
        0

interpret :: Program -> IO ()
interpret (Program p) = do
    res <- runInterpreter p
    case res of
        (Left err) -> error "TODO"
        _          -> return ()
-- interpret p = putStrLn "no interpreter yet"
