-- {-# LANGUAGE FlexibleContexts #-}
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

-- TODO: showS errors???

------------------EXPR---------------------------------------------------------

getVarValFromArgsAndExpr args es = do
    let tve = zip3 (map fst args) (map snd args) es
    mapM
        (\(t, var, exp) -> do
            val <- getValAndCheckType t exp
            return (var, val)
        )
        tve

getVarLocs :: [Arg] -> [Expr] -> IMon Env
getVarLocs args es = do
    list <- zipWithM matchArgExpr args es
    return $ M.fromList list
  where
    matchArgExpr :: Arg -> Expr -> IMon (Var, Loc)
    matchArgExpr (Arg t (Ident var)) e = do
        val <- getValAndCheckType t e
        loc <- alloc val
        putNewVal loc val
        return (var, loc)
    matchArgExpr (RefArg t (Ident var)) e@(EVar (Ident var')) = do
        _   <- getValAndCheckType t e
        loc <- getLoc var'
        return (var, loc)
    matchArgExpr (RefArg t var) _ = throwM
        "Only variable arguments are allowed for reference arguments."

evalExprM :: Expr -> IMon Val
evalExprM (EVar (Ident var)           ) = readVal var

evalExprM (ELambda args ret (Block ss)) = do
    env <- ask
    return $ VFun args ret env ss

evalExprM (EApp (Ident name) es) = do
    (VFun args ret clos body) <- readVal name
    guard (length es == length args) -- TODO: czy długości się zgadzają!!!!! throwM!

    varLocs <- getVarLocs args es

    let env' = addMany clos varLocs
    -- Type checker ensures return's type and existence, so here pattern match is sufficient
    (_, Just (R val)) <- local (const env') (execStmtsM body)
    return val

evalExprM (EString s) = (return . VString) s
evalExprM (ELitInt n) = (return . VInt) n
evalExprM ELitTrue    = (return . VBool) True
evalExprM ELitFalse   = (return . VBool) False

evalExprM (Neg e)     = do
    e' <- evalExprM e
    case e' of
        (VInt n) -> (return . VInt . negate) n
        _        -> throwM "TODO Inappropriate type for integer negation"

evalExprM (Not e) = do
    e' <- evalExprM e
    case e' of
        (VBool b) -> (return . VBool . not) b
        _         -> throwM "TODO Inappropriate type for bool negation"


evalExprM (EMul e symbol f) = do
    e' <- evalExprM e
    f' <- evalExprM f
    return $ performMulOp symbol e' f'

evalExprM (EAdd e symbol f) = do
    e' <- evalExprM e
    f' <- evalExprM f
    return $ performAddOp symbol e' f'

evalExprM (ERel e symbol f) = do
    e' <- evalExprM e
    f' <- evalExprM f
    return $ performRelOp symbol e' f'

evalExprM (EAnd e f) = do --TODO: lazy evaluation????????
    e' <- evalExprM e
    f' <- evalExprM f
    return $ performAndOr (&&) e' f'

evalExprM (EOr e f) = do
    e' <- evalExprM e
    f' <- evalExprM f
    return $ performAndOr (||) e' f'

evalExprM e = do
    liftIO $ putStrLn $ printTree e
    error "evalExprM e"

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
performAddOp Plus (VString a) (VString b) = VString $ a ++ b
performAddOp op   (VInt    a) (VInt    b) = VInt $ addOp op a b
performAddOp _    _           _           = error "performAddOp"

addOp :: AddOp -> Integer -> Integer -> Integer
addOp Plus  = (+)
addOp Minus = (-)

performRelOp :: RelOp -> Val -> Val -> Val
performRelOp EQU a        b        = VBool $ a == b -- TODO: handle all types gurl!
performRelOp NE  a        b        = VBool $ a /= b -- TODO: ^
performRelOp op  (VInt a) (VInt b) = VBool $ relOp op a b
performRelOp _   _        _        = error "performRelOp"

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

----------------------STMTS----------------------------------------------------

getValAndCheckType :: Type -> Expr -> IMon Val
getValAndCheckType t e = do
    val <- evalExprM e
    if t == valToType val
        then return val
        else
            throwM
            $  "Expression \""
            ++ show e
            ++ "\" doesn't match type: "
            ++ show t

continueInCurrentEnv :: IMon (Env, Flow)
continueInCurrentEnv = do
    env <- ask
    return (env, Nothing)

valToString :: ReturnVal -> String
valToString res = case res of
    (Just val) -> case val of
        (VInt    x) -> show x
        (VBool   b) -> show b
        (VString s) -> s
        _           -> error "ERROR valToString"
    Nothing -> error "ERROR Nothing valToString"


execStmtM :: Stmt -> IMon (Env, Flow)
execStmtM Empty    = continueInCurrentEnv
execStmtM (SExp e) = do
    e' <- evalExprM e
    continueInCurrentEnv

execStmtM (SPrint e) = do
    val <- evalExprM e
    if valToType val `notElem` [Int, Bool, Str]
        then throwM $ "print: invalid type of expression: " ++ show
            (valToType val)
        else do
            liftIO $ print val
            continueInCurrentEnv

execStmtM VRet = do
    env <- ask
    return (env, Just (R VVoid))

execStmtM (Ret e) = do
    val <- evalExprM e
    env <- ask
    return (env, Just (R val))

execStmtM (FnDef ret (Ident name) args (Block ss)) = do
    env <- ask
    let val = VFun args ret env ss
    loc <- alloc val
    let env' = M.insert name loc env
    let val' = VFun args ret env' ss
    putNewVal loc val'
    return (env', Nothing)

execStmtM (Decl _ []) =
    throwM "Miracle, that should never get through parsing phase."
execStmtM (Decl Void _) =
    throwM $ "Illegal type for variable declaration: " ++ show Void
execStmtM (Decl (Fun _ _) (DefaultInit _ : _)) =
    throwM "Function declaration without initialisation is forbidden."

execStmtM (Decl t ds) = do
    env <- ask
    foldM go (env, Nothing) ds
  where
    go (possiblyUpdatedEnv, ret) d =
        local (const possiblyUpdatedEnv) $ handleDecl d
    handleDecl d = do
        (var, loc) <- declToPair d
        env        <- ask
        let env' = M.insert var loc env -- TODO: musi być snd? 
        return (env', Nothing)

    declToPair :: Item -> IMon (Var, Loc)
    declToPair d = do
        (var, val) <- unpackDeclaration d
        loc        <- alloc val -- NIGDZIE NIE SPRAWDZAM CZY NIE MA JUŻ ZMIENNEJ Z TYM ID, NIEDOBRZE NO
        putNewVal loc val
        return (var, loc)

    unpackDeclaration :: Item -> IMon (Var, Val)
    unpackDeclaration (DefaultInit (Ident var)) =
        (return . (,) var) $ case t of
            Int  -> VInt 0
            Bool -> VBool False
            Str  -> VString ""
    unpackDeclaration (Init (Ident var) e) = do
        val <- getValAndCheckType t e
        return (var, val)

execStmtM (Incr (Ident var)) = do
    val <- readVal var
    case val of
        (VInt n) -> do
            changeVal var (VInt (n + 1))
            continueInCurrentEnv
        _ -> throwM "Illegal type to increment."

-- TODO: 90% takie jak Incr
execStmtM (Decr (Ident var)) = do
    val <- readVal var
    case val of
        (VInt n) -> do
            changeVal var (VInt (n - 1))
            continueInCurrentEnv
        _ -> throwM "Illegal type to increment."

execStmtM (Ass (Ident var) e) = do
    val <- evalExprM e
    changeVal var val
    continueInCurrentEnv

        -- let env' = M.insert var loc env
        -- local (const env') (execStmtM (Decl t items))

execStmtM Continue = do
    env <- ask
    return (env, Just Cont)

execStmtM Break = do
    env <- ask
    return (env, Just Br)

execStmtM while@(While e s) = do
    cond <- evalExprM e
    case cond of
        (VBool True) -> do
            (_, ret) <- execStmtM s
            case ret of
                (Just Br) -> do
                    liftIO $ putStrLn "BREAK"
                    continueInCurrentEnv
                _ -> execStmtM while
        (VBool False) -> continueInCurrentEnv
        _ ->
            throwM
                $  "Illegal expression for while loop condition: " -- TODO: nazwa while jest malo uniwersAlna xD
                ++ show e


    -- execCondAndActM e continueWhile continueInCurrentEnv

execStmtM (Cond e s) = execCondAndActM e (execStmtM s) continueInCurrentEnv

execStmtM (CondElse e s1 s2) = execCondAndActM e (execStmtM s1) (execStmtM s2)

execStmtM (BStmt (Block ss)) = execStmtsM ss

execStmtM e                  = do
    liftIO $ putStrLn $ printTree e
    error "execStmtM e"

execCondAndActM
    :: Expr -> IMon (Env, Flow) -> IMon (Env, Flow) -> IMon (Env, Flow)
execCondAndActM e tFun fFun = do
    cond <- evalExprM e
    case cond of
        (VBool True ) -> tFun
        (VBool False) -> fFun
        _ ->
            throwM
                $  "Illegal expression for while loop condition: " -- TODO: nazwa while jest malo uniwersAlna xD
                ++ show e


execStmtsM :: [Stmt] -> IMon (Env, Flow)
execStmtsM ss = do
    env <- ask
    foldM go (env, Nothing) ss
  where
    go (possiblyUpdatedEnv, ret) s = case ret of
        Nothing -> local (const possiblyUpdatedEnv) (execStmtM s) -- always run in 'new' env, which is only sometimes changed
        _       -> do
            liftIO $ putStrLn "Value's been returned~  "
            return (possiblyUpdatedEnv, ret)


runInterpreter (Program prog) = runStateT
    (runReaderT (execStmtsM prog) M.empty)
    initialState
    where initialState = IMState M.empty 0
