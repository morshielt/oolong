-- {-# LANGUAGE FlexibleContexts #-}
module Interpreter
    ( runInterpreter
    )
where

import           AbsOolong

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

getVarLocs :: [Arg] -> [Expr] -> IMon VarToLoc
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

evalExprM :: Expr -> IMon ReturnVal
evalExprM (EVar (Ident var)           ) = (return . Just) =<< readVal var

evalExprM (ELambda args ret (Block ss)) = do -- TODO: no lambda recursion!
    env <- ask
    (return . Just) $ VFun args ret env ss
    -- let val = VFun args ret env ss
    -- loc <- alloc val
    -- let env' = M.insert name loc env
    -- let val' = VFun args ret env' ss
    -- putNewVal loc val'
    -- return (env', Nothing)

evalExprM (EApp (Ident name) es) = do
    (VFun args ret clos body) <- readVal name
    guard (length es == length args) -- TODO: czy długości się zgadzają!!!!! throwM!

    varLocs <- getVarLocs args es

    let env' = M.unionWith (curry snd) clos varLocs
    (_, retVal) <- local (const env') (execStmtsM body)

    case retVal of
        Nothing ->
            throwM
                $  name
                ++ ": Invalid return type. Expected: "
                ++ show ret
                ++ " Got: "
                ++ show Void
        (Just val) -> if valToType val == ret
            then return retVal
            else
                throwM
                $  name
                ++ ": Invalid return type. Expected: "
                ++ show ret
                ++ " Got: "
                ++ show (valToType val)
--   where
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
    e' <- evalExprM e
    case e' of
        (Just val) -> if t == valToType val
            then return val
            else
                throwM
                $  "Expression \""
                ++ show e
                ++ "\" doesn't match type: "
                ++ show t
        Nothing ->
            throwM $ "Illegal expression for variable declaration: " ++ show e


-- TODO: nigdzie się jeszcze nie sprawdza, 
-- czy w danym scope nie ma już czasem zmiennej o takiej nazwie!

continueInCurrentEnv :: IMon (VarToLoc, ReturnVal)
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


execStmtM :: Stmt -> IMon (VarToLoc, ReturnVal)
execStmtM Empty    = continueInCurrentEnv
execStmtM (SExp e) = do
    e' <- evalExprM e
    case e' of
        (Just _) -> continueInCurrentEnv
        Nothing  -> throwM $ "SExp: invalid expression " ++ show e
execStmtM (SPrint e) = do
    e' <- evalExprM e
    case e' of
        (Just val) -> if valToType val `notElem` [Int, Bool, Str]
            then throwM $ "print: invalid type of expression: " ++ show
                (valToType val)
            else do
                liftIO $ print val
                continueInCurrentEnv
        Nothing -> throwM $ "print: invalid expression " ++ show e


execStmtM VRet = do
    env <- ask
    return (env, Just VVoid)

execStmtM (Ret e) = do
    e' <- evalExprM e
    case e' of
        (Just val) -> do
            env <- ask
            return (env, e')
        Nothing -> throwM $ "Illegal expression to return: " ++ show e

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
    ds' <- mapM declToPair ds
    let decls = M.fromList ds'
    env <- ask
    let env' = M.unionWith (curry snd) env decls -- TODO: musi być snd? 
    return (env', Nothing)
  where
    -- w sensie teoretycznie chyba i tak nie powinno być zmiennych z tą samą nazwą 
    -- w tym samym bloku, ale wydaje mi się, że to się przyda to nadpisywania w kolejnych blokach,
    -- nie wiem
    declToPair :: Item -> IMon (Var, Loc)
    declToPair d = do
        (var, val) <- unpackDeclaration d
        loc        <- alloc val -- NIGDZIE NIE SPRAWDZAM CZY NIE MA JUŻ ZMIENNEJ Z TYM ID, NIEDOBRZE NO
        putNewVal loc val
        return (var, loc)       -- TODO: ew. bez typechecka to: dodać zmienną z nr-em scope i mapę var->nr_scope "w jakim scope ostatnie zadekralowaną tą zmienną"
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
    e' <- evalExprM e
    case e' of
        (Just val) -> do
            changeVal var val
            continueInCurrentEnv
        Nothing ->
            throwM $ "Illegal expression for variable assignment: " ++ show e

        -- let env' = M.insert var loc env
        -- local (const env') (execStmtM (Decl t items))


execStmtM (While e s) = execCondAndActM e continueWhile continueInCurrentEnv
  where
    continueWhile = do
        execStmtM s
        execStmtM (While e s)

execStmtM (Cond e s) = execCondAndActM e (execStmtM s) continueInCurrentEnv

execStmtM (CondElse e s1 s2) = execCondAndActM e (execStmtM s1) (execStmtM s2)

execStmtM (BStmt (Block ss)) = do
    env <- ask
    local (const env) (execStmtsM ss) -- TODO NIE WIEM C>Y TO JEST DOBRZE (W SENSIE VAR. SHADOW)
    return (env, Nothing)

execStmtM _ = error "Not implemented yet or error xD"

execCondAndActM
    :: Expr
    -> IMon (VarToLoc, ReturnVal)
    -> IMon (VarToLoc, ReturnVal)
    -> IMon (VarToLoc, ReturnVal)
execCondAndActM e tFun fFun = do
    b <- evalExprM e
    case b of
        (Just cond) -> case cond of
            (VBool True ) -> tFun
            (VBool False) -> fFun
            _ ->
                throwM
                    $  "Illegal expression for while loop condition: " -- TODO: nazwa while jest malo uniwersAlna xD
                    ++ show e
        Nothing ->
            throwM $ "Invalid expression for while loop condition: " ++ show e


execStmtsM :: [Stmt] -> IMon (VarToLoc, ReturnVal)
execStmtsM []       = continueInCurrentEnv
execStmtsM (s : xs) = do
    (possiblyUpdatedEnv, ret) <- execStmtM s
    case ret of
        Nothing ->
            -- liftIO $ putStrLn "NOTHINGrETURNED~  "
            local (const possiblyUpdatedEnv) (execStmtsM xs) -- always run in 'new' env, which is only sometimes changed
        _ -> do
            liftIO $ putStrLn "Value's been returned~  "
            return (possiblyUpdatedEnv, ret) -- (?) function return (?)

runInterpreter (Program prog) = runStateT
    (runReaderT (execStmtsM prog) M.empty)
    initialState
    where initialState = IMState M.empty 0
