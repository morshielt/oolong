module Expressions where

import           Control.Monad.Except

import AbsOolong

import Types
import Utils

evalExprM :: Expr -> IMon ReturnVal
evalExprM (EVar    (Ident var)) = (return . Just) =<< readVal var

evalExprM (EString s          ) = (return . Just . VString) s
evalExprM (ELitInt n          ) = (return . Just . VInt) n
evalExprM ELitTrue              = (return . Just . VBool) True
evalExprM ELitFalse             = (return . Just . VBool) False
evalExprM (Neg e)               = do
    e' <- evalExprM e
    case e' of
        (Just e'') -> case e'' of
            (VInt n) -> (return . Just . VInt . negate) n
            _        -> throwM "TODO Inappropriate type for integer negation"
        Nothing -> throwM "TODO Nothing to integer negate"

evalExprM (Not e) = do
    e' <- evalExprM e
    case e' of
        (Just e'') -> case e'' of
            (VBool b) -> (return . Just . VBool . not) b
            _         -> throwM "TODO Inappropriate type for bool negation"
        Nothing -> throwM "TODO Nothing to bool negate"


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

evalExprM (EAnd e f) = do
    e' <- evalExprM e
    f' <- evalExprM f
    return $ liftM2 (performAndOr (&&)) e' f'

evalExprM (EOr e f) = do
    e' <- evalExprM e
    f' <- evalExprM f
    return $ liftM2 (performAndOr (||)) e' f'

-- TODO: FIXME: errory złe
performAndOr :: (Bool -> Bool -> Bool) -> Val -> Val -> Val
performAndOr op (VBool a) (VBool b) = VBool $ op a b
performAndOr _  _         _         = error "performAndOr"

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
-- performArithmOp _ _    _    = error "TODO Invalid performArithmOp"

-- performRelationOp :: RelationOp o => o -> Val -> Val -> Val
-- performRelationOp o (VInt a) (VInt b) = VBool $ relOp o a b
-- performRelationOp _ _    _    = error "TODO Invalid performRelationOp"


-- performArithmOp :: Op -> Val -> Val -> Val
-- performArithmOp relOp a b = undefined
-- performArithmOp _ _ _ = error "TODO Invalid operation"
