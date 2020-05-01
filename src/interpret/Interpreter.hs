module Interpreter
    ( runInterpreter
    )
where

import           AbsOolong
import           PrintOolong

import           Types
import           Utils

import           Data.Map                      as M
                                         hiding ( map )

import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Except

------------------EXPR---------------------------------------------------------

evalExprM :: Expr -> IM Val
evalExprM (EVar (Ident var)           ) = readVar var

evalExprM (ELambda args ret (Block ss)) = do
    env <- ask
    return $ VFun args ret env ss

evalExprM (EApp (Ident name) es) = do
    (VFun args _ env ss) <- readVar name
    env'                 <- flip M.union env <$> applyArgs args es
    (_, Just (R val))    <- local (const env') (execStmtsM ss)
    return val
  where
    applyArgs :: [Arg] -> [Expr] -> IM Env
    applyArgs args es = M.fromList <$> zipWithM matchArgExpr args es
      where
        matchArgExpr :: Arg -> Expr -> IM (Var, Loc)
        matchArgExpr (Arg t (Ident var)) e = do
            val <- evalExprM e
            loc <- alloc
            putVal loc val
            return (var, loc)
        matchArgExpr (RefArg t (Ident var)) e@(EVar (Ident var')) = do
            _   <- evalExprM e
            loc <- getLoc var'
            return (var, loc)

evalExprM (EString s) = (return . VString) s
evalExprM (ELitInt n) = (return . VInt) n
evalExprM ELitTrue    = (return . VBool) True
evalExprM ELitFalse   = (return . VBool) False

evalExprM (Neg e)     = do
    (VInt n) <- evalExprM e
    (return . VInt . negate) n

evalExprM (Not e) = do
    (VBool b) <- evalExprM e
    (return . VBool . not) b

evalExprM (EMul e Div f) = do
    (e', f') <- evalExprM2 e f
    when (f' == VInt 0) $ throwM "Division by 0"
    return $ mulOp Div e' f'

evalExprM (EMul e op f) = performBinOpM (mulOp op) e f
evalExprM (EAdd e op f) = performBinOpM (addOp op) e f
evalExprM (ERel e op f) = performBinOpM (relOp op) e f
evalExprM (EAnd e f   ) = do
    e' <- evalExprM e
    if e' == VBool False then return e' else evalExprM f
evalExprM (EOr e f) = do
    e' <- evalExprM e
    if e' == VBool True then return e' else evalExprM f

evalExprM2 e f = do
    e' <- evalExprM e
    f' <- evalExprM f
    return (e', f')

performBinOpM :: (Val -> Val -> Val) -> Expr -> Expr -> IM Val
performBinOpM op e f = do
    (e', f') <- evalExprM2 e f
    return $ op e' f'

mulOp :: MulOp -> Val -> Val -> Val
mulOp Times (VInt a) (VInt b) = VInt $ a * b
mulOp Div   (VInt a) (VInt b) = VInt $ a `div` b
mulOp Mod   (VInt a) (VInt b) = VInt $ a `mod` b

addOp :: AddOp -> Val -> Val -> Val
addOp Plus  (VString a) (VString b) = VString $ a ++ b
addOp Plus  (VInt    a) (VInt    b) = VInt $ a + b
addOp Minus (VInt    a) (VInt    b) = VInt $ a - b

relOp :: RelOp -> Val -> Val -> Val
relOp LTH (VInt a) (VInt b) = VBool $ a < b
relOp LE  (VInt a) (VInt b) = VBool $ a <= b
relOp GTH (VInt a) (VInt b) = VBool $ a > b
relOp GE  (VInt a) (VInt b) = VBool $ a >= b
relOp EQU a        b        = VBool $ a == b
relOp NE  a        b        = VBool $ a /= b

----------------------STMTS----------------------------------------------------

continueM :: IM (Env, Flow)
continueM = do
    env <- ask
    return (env, Nothing)

execStmtM :: Stmt -> IM (Env, Flow)
execStmtM Empty      = continueM
execStmtM (SExp   e) = evalExprM e >> continueM
execStmtM (SPrint e) = evalExprM e >>= liftIO . print >> continueM

execStmtM VRet       = liftM2 (,) ask $ return (Just (R VVoid))
execStmtM (Ret e)    = liftM2 (,) ask $ Just . R <$> evalExprM e

execStmtM (FnDef ret (Ident name) args (Block ss)) = do
    env <- ask
    loc <- alloc
    let env' = M.insert name loc env
    let val' = VFun args ret env' ss
    putVal loc val'
    return (env', Nothing)

execStmtM (Decl t ds) = do
    env <- ask
    flip (,) Nothing <$> foldM go env ds
  where
    go :: Env -> Item -> IM Env
    go env d = local (const env) $ unpackDecl d >>= uncurry declare
      where
        unpackDecl :: Item -> IM (Var, Val)
        unpackDecl (Init (Ident var) e     ) = (,) var <$> evalExprM e
        unpackDecl (DefaultInit (Ident var)) = return (var, defaultVal t)

execStmtM (Incr (Ident var)) = do
    (VInt n) <- readVar var
    changeVal var (VInt (n + 1))
    continueM

execStmtM (Decr (Ident var)) = do
    (VInt n) <- readVar var
    changeVal var (VInt (n - 1))
    continueM

execStmtM (Ass (Ident var) e) = do
    val <- evalExprM e
    changeVal var val
    continueM

execStmtM Continue          = flip (,) (Just Cont) <$> ask
execStmtM Break             = flip (,) (Just Br) <$> ask

execStmtM while@(While e s) = execCondAndActM e continueWhile continueM
  where
    continueWhile = do
        (_, ret) <- execStmtM s
        case ret of
            (Just Br) -> continueM
            _         -> execStmtM while

execStmtM (Cond e s        ) = execCondAndActM e (execStmtM s) continueM
execStmtM (CondElse e s1 s2) = execCondAndActM e (execStmtM s1) (execStmtM s2)

execStmtM (BStmt (Block ss)) = execStmtsM ss

execCondAndActM :: Expr -> IM (Env, Flow) -> IM (Env, Flow) -> IM (Env, Flow)
execCondAndActM e trueM falseM = do
    cond <- evalExprM e
    case cond of
        (VBool True ) -> trueM
        (VBool False) -> falseM

execStmtsM :: [Stmt] -> IM (Env, Flow)
execStmtsM ss = do
    env <- ask
    foldM go (env, Nothing) ss
  where
    go (possiblyUpdatedEnv, ret) s = case ret of
        Nothing -> local (const possiblyUpdatedEnv) (execStmtM s) -- always run in 'new' env, which is only sometimes changed
        _       -> return (possiblyUpdatedEnv, ret)

runInterpreter (Program prog) = runStateT
    (runReaderT (execStmtsM prog) M.empty)
    initialState
    where initialState = IMState M.empty 0
