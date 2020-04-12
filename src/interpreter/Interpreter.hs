-- {-# LANGUAGE FlexibleContexts #-}
module Interpreter where

import           AbsOolong

import           Types

import           Data.Map                      as M
import           Data.Maybe

import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Except
import           Control.Monad.Trans.Except

throwM :: String -> IMon a
throwM = lift . lift . throwE

alloc :: Val -> IMon Loc
alloc val = do
    modify
        (\st ->
            let mod = M.insert (freeLoc st + 1) val (locToVal st)
            in  st { freeLoc = freeLoc st + 1, locToVal = mod }
        )
    gets freeLoc

continueInCurrentEnv :: IMon (VarToLoc, ReturnResult)
continueInCurrentEnv = do
    env <- ask
    return (env, Nothing)

evalExprM :: Expr -> IMon ReturnResult
evalExprM (EVar (Ident var)) = do  -- TODO: JAK NOT FOUND TO DAJE NOTHING TO JAKOŚ MA SIĘ WYJEBYWAĆ
    env   <- ask
    state <- gets locToVal
    -- res   <- getVal env state
    case getVal env state of
        Nothing -> throwM "evalExprM: Location's value not found!!!"
        _       -> return $ getVal env state
  where
    getVal :: VarToLoc -> LocToVal -> Maybe Val
    getVal env state = case M.lookup var env of
        Nothing -> error "evalExprM:getVal: Variable location not found!!!"
        _       -> do
            loc <- M.lookup var env
            -- error $ show loc
            M.lookup loc state

evalExprM (EString s) = (return . Just . VString) s
evalExprM (ELitInt n) = (return . Just . VInt) n
evalExprM ELitTrue    = (return . Just . VBool) True
evalExprM ELitFalse   = (return . Just . VBool) False
evalExprM (Neg e)     = do
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


execStmtM :: Stmt -> IMon (VarToLoc, ReturnResult)
execStmtM Empty      = continueInCurrentEnv

execStmtM (SPrint e) = do
    e' <- evalExprM e
    liftIO $ putStrLn $ valToString e'
    continueInCurrentEnv

execStmtM (Decl _ []) = continueInCurrentEnv
execStmtM (Decl t (DefaultInit (Ident var) : items)) = --TODO: kiedyś odrekurencyjnić? XD
    if t `notElem` [Int, Bool, Str]
        then throwM $ "Illegal type for default declaration: " ++ show t
        else do
            loc <- alloc $ case t of
                Int  -> VInt 0
                Bool -> VBool False
                Str  -> VString ""
            env <- ask -- TODO: tu się powtarza to co niżej~
            let env' = M.insert var loc env
            local (const env') (execStmtM (Decl t items))

execStmtM (Decl t (Init (Ident var) e : items)) = if t == Void
    then throwM $ "Illegal type for variable declaration: " ++ show Void
    else do
        e' <- evalExprM e
        case e' of
            (Just val) -> 
              if t == valToType val then do
                loc <- alloc val
                env <- ask
                let env' = M.insert var loc env
                local (const env') (execStmtM (Decl t items))
              else throwM $ "Expression \"" ++ show e ++ "\" doesn't match type: " ++ show t
            Nothing -> throwM $ "Illegal expression for variable declaration: " ++ show e

execStmtM _ = undefined

-- itemsToVarVal :: Type -> [Item] -> IMon ([(Var, Val)])
-- itemsToVarVal t = return Prelude.map (toVarVal t)

-- toVarVal :: Type -> Item -> IMon () -> (Var, Val)
-- toVarVal Int  (DefaultInit (Ident var)     ) = (var, VInt 0)
-- toVarVal Bool (DefaultInit (Ident var)     ) = (var, VBool False)
-- toVarVal Str  (DefaultInit (Ident var)     ) = (var, VString "")
-- toVarVal Void (DefaultInit _) = error "VOID DEFAULT DECLARATION"
-- toVarVal _ (DefaultInit _) = error "DEFAULT FUNC DECL ILLEGAL"

-- toVarVal Int  (Init (Ident var) (ELitInt val)) = (var, VInt val)
-- toVarVal Bool (Init (Ident var) ELitTrue   ) = (var, VBool True)
-- toVarVal Bool (Init (Ident var) ELitFalse  ) = (var, VBool False)
-- Decl Bool [Init (Ident "j") (ELitInt 1)]
-- data Type = Int | Str | Bool | Void | Fun [Type] Type

valToString :: ReturnResult -> String
valToString res = case res of
    (Just val) -> case val of
        (VInt    x) -> show x
        (VBool   b) -> show b
        (VString s) -> s
        _           -> error "ERROR valToString"
    Nothing -> error "ERROR Nothing valToString"
-- valToString (VInt  x) = show x
-- valToString (VBool   b) = show b
-- valToString (VString s) = s
-- valToString _       = error "ERROR TODO"


-- valToString (VInt  x) = show x
-- valToString (VBool   b) = show b
-- valToString (VString s) = s
-- valToString _       = "ERROR TODO"

  -- do
  -- env <- ask
  -- return (env, e')

-- execStmtM (Decl e) = do
--   env <- ask
--   let e' = evalExprM e env
--   modify (Map.insert x e')

-- TODO: to jest jakoś chujowo -- TODO: jednak jest kul
interpretMany :: [Stmt] -> IMon (VarToLoc, ReturnResult)
interpretMany []       = continueInCurrentEnv
interpretMany (s : xs) = do
    (possiblyUpdatedEnv, ret) <- execStmtM s
    case ret of
        Nothing -> 
            -- liftIO $ putStrLn "NOTHINGrETURNED~  "
            local (const possiblyUpdatedEnv) (interpretMany xs) -- always run in 'new' env, which is only sometimes changed
        _ -> return (possiblyUpdatedEnv, ret) -- (?) function return (?)

runInterpreter (Program prog) = runStateT
    (runReaderT (interpretMany prog) M.empty)
    initialState
    where initialState = IMState M.empty 0

-- interpret :: Program -> IO ()
-- interpret (Program p) = do
--   res <- runInterpreter p
--   case res of
--     (Left err) -> error "TODO"
--     _      -> return ()
-- interpret p = putStrLn "no interpreter yet"
