module TypeCheck
    ( runTypeChecker
    )
where

import           AbsOolong
import           PrintOolong

import           TCTypes
import           TCUtils

import           Control.Monad                  ( when )
import           Control.Monad.Reader
import           Control.Monad.Except

import           Data.Map                      as M
                                         hiding ( map )


matchType :: [TCType] -> TCType -> TCM ()
matchType [ex] act = when (ex /= act)
    $ throwMsg ["Expected type:", show ex, "\nActual type:", show act]
matchType exs act = when (act `notElem` exs) $ throwMsg
    ["Expected one of types:", show' exs, "\nActual type:", show act]

matchExpType :: TCType -> Expr -> TCM TCType
matchExpType ex e = do
    act <- checkExprM e
    when (ex /= act) $ throwMsg
        [ "Expected type:"
        , show ex
        , "\nActual type:"
        , show act
        , "\nin expression: "
        , printTree e
        ]
    return act

checkBinOp :: [TCType] -> Expr -> Expr -> Expr -> TCM TCType
checkBinOp ts e1 e2 expr = matchBinOpTypes ts e1 e2 `throwExtraMsg` msg
  where
    matchBinOpTypes :: [TCType] -> Expr -> Expr -> TCM TCType
    matchBinOpTypes ts e1 e2 = do
        e1T <- checkExprM e1
        matchType ts e1T
        matchType [e1T] =<< checkExprM e2
        return e1T

    msg err = [err, "\nIn:", printTree expr]


checkExprM :: Expr -> TCM TCType
checkExprM expr@(ELambda args ret bs@(Block ss)) = do
    let ret' = typeToTCType ret

    argsTypes <- handleArgs args `throwExtraMsg` msg
    env       <- ask
    let envWithArgs = env { types       = M.union argsTypes (types env)
                          , expectedRet = Just (ret', "lambda expression")
                          }

    local (const envWithArgs) $ checkStmtM (BStmt bs)
    return $ TFun (map argToTCArg args) ret'
    where msg e = [e, "\nin lambda expression:\n", printTree expr]

checkExprM (ELitInt _)          = return TInt
checkExprM ELitTrue             = return TBool
checkExprM ELitFalse            = return TBool
checkExprM (  EString _       ) = return TString

checkExprM (  Not     e       ) = matchExpType TBool e
checkExprM (  Neg     e       ) = matchExpType TInt e
checkExprM e@(EMul e1 _     e2) = checkBinOp [TInt] e1 e2 e
checkExprM e@(EAdd e1 Plus  e2) = checkBinOp [TInt, TString] e1 e2 e
checkExprM e@(EAdd e1 Minus e2) = checkBinOp [TInt] e1 e2 e

checkExprM e@(ERel e1 EQU e2) =
    checkBinOp [TInt, TString, TBool] e1 e2 e >> return TBool
checkExprM e@(ERel e1 NE e2) =
    checkBinOp [TInt, TString, TBool] e1 e2 e >> return TBool
checkExprM e@(ERel e1 _ e2) = checkBinOp [TInt] e1 e2 e >> return TBool

checkExprM e@(   EAnd e1 e2         ) = checkBinOp [TBool] e1 e2 e
checkExprM e@(   EOr  e1 e2         ) = checkBinOp [TBool] e1 e2 e
checkExprM (     EVar (Ident var)   ) = getVarType var

checkExprM expr@(EApp (Ident var) es) = do
    typeScope <- getVarTypeScope var
    case typeScope of
        Nothing -> throwMsg ["function ", var, " is not declared"]
        (Just (TFun args ret, s)) -> do
            checkArgs args es `throwExtraMsg` msg
            return ret
        (Just _) -> throwMsg [var, " is not a function"]
  where
    msg e = [var, ":", e, "\nin function call:", printTree expr]
    checkArgs :: [TCArg] -> [Expr] -> TCM ()
    checkArgs args es = if length args == length es
        then mapM_ checkArg $ zip args es
        else throwTCM "Invalid number of arguments in function call"
      where
        checkArg (R t, e@(EVar var)) = matchExpType t e
        checkArg (R t, _) = throwTCM "Reference argument must be a variable"
        checkArg (V t, e) = matchExpType t e



checkStmtM :: Stmt -> TCM TCEnv
checkStmtM Empty    = ask

checkStmtM (SExp e) = do
    t <- checkExprM e
    ask

checkStmtM stmt@(SPrint e) = do
    t <- checkExprM e
    when (t `notElem` [TString, TInt, TBool]) $ throwMsg
        ["Cannot print type ", show t, "in statement:\n", printTree stmt]
    ask

checkStmtM (Ass (Ident var) e) = do
    t <- getVarType var
    matchExpType t e
    ask

checkStmtM (Decl t ds) = do
    when (t == Void) $ throwTCM "Void variable declaration is forbidden"
    env <- ask
    foldM go env ds
  where
    go :: TCEnv -> Item -> TCM TCEnv
    go acc d = local (const acc) $ handleDecl (typeToTCType t) d

    handleDecl :: TCType -> Item -> TCM TCEnv
    handleDecl t d = do
        var <- case d of
            (DefaultInit (Ident var)) -> case t of
                (TFun _ _) -> throwMsg
                    [var, ": Default function declaration is forbidden"]
                _ -> return var
            (Init (Ident var) e) -> matchExpType t e >> return var
        checkIfNameAlreadyInScope var

        scope <- asks scope
        env   <- ask
        let envWithDecl = M.insert var (t, scope) (types env)
        return $ env { types = envWithDecl }

checkStmtM stmt@(Incr (Ident var)) = checkIncrDecr var stmt
checkStmtM stmt@(Decr (Ident var)) = checkIncrDecr var stmt

checkStmtM stmt@(While e s       ) = do
    matchExpType TBool e `throwExtraMsg` msg
    local (\env -> env { inLoop = True }) (checkStmtM s)
    ask
    where msg e = [e, "\nin loop:", printTree stmt]

checkStmtM stmt@(Cond e s) = do
    matchExpType TBool e `throwExtraMsg` msg
    checkStmtM s
    ask
    where msg e = [e, "\nin if statement:", printTree stmt]

checkStmtM stmt@(CondElse e s1 s2) = do
    matchExpType TBool e `throwExtraMsg` msg
    checkStmtM s1
    checkStmtM s2
    ask
    where msg e = [e, "\nin if/else statement:", printTree stmt]

checkStmtM (BStmt (Block ss)) = do
    env <- ask
    s   <- asks scope
    local (\env -> env { scope = s + 1 }) (checkStmtsM ss)
    ask

checkStmtM VRet         = matchReturn TVoid
checkStmtM (     Ret e) = matchReturn =<< checkExprM e

checkStmtM stmt@(FnDef ret (Ident name) args bs@(Block ss)) = do
    checkIfNameAlreadyInScope name

    let ret' = typeToTCType ret
    scope <- asks scope
    env   <- ask
    let t    = TFun (map argToTCArg args) ret'
    let fEnv = env { types = M.insert name (t, scope) (types env) }

    argsTypes <- handleArgs args `throwExtraMsg` msg
    let fEnvWithArgs = fEnv { types       = M.union argsTypes (types fEnv)
                            , expectedRet = Just (ret', name)
                            }
    local (const fEnvWithArgs) $ checkStmtM (BStmt bs)
    return fEnv
  where
    msg e = [name, ": ", e, "\nin function definition:\n", printTree stmt]

checkStmtM Break = do
    loop <- asks inLoop
    if loop then ask else throwTCM "Break outside of a loop"

checkStmtM Continue = do
    loop <- asks inLoop
    if loop then ask else throwTCM "Continue outside of a loop"


checkIncrDecr :: Var -> Stmt -> TCM TCEnv
checkIncrDecr var stmt = do
    t <- getVarType var
    matchType [TInt] t `throwExtraMsg` msg
    ask
    where msg e = [e, "in statement:", printTree stmt]

matchReturn :: TCType -> TCM TCEnv
matchReturn t = do
    ex <- asks expectedRet
    case ex of
        Nothing           -> throwTCM "Return outside of function"
        (Just (eT, name)) -> matchType [eT] t `throwExtraMsg` msg name
    ask
    where msg name e = [name, ":", e, "in function return"]

handleArgs :: [Arg] -> TCM Types
handleArgs args = do
    scope <- asks scope
    let list = map
            (\arg -> case arg of
                (Arg    t (Ident var)) -> (var, (typeToTCType t, scope + 1))
                (RefArg t (Ident var)) -> (var, (typeToTCType t, scope + 1))
            )
            args
    let mapList = M.fromList list
    if length list == length mapList
        then return mapList
        else throwTCM "Function arguments must have different names"

checkStmtsM :: [Stmt] -> TCM TCEnv
checkStmtsM ss = do
    env <- ask
    foldM go env ss
  where
    go :: TCEnv -> Stmt -> TCM TCEnv
    go env' s = local (const env') $ checkStmtM s

checkReturns :: [Stmt] -> TCM ()
checkReturns = mapM_ checkReturn
  where
    checkExprReturn :: Expr -> TCM ()
    checkExprReturn e@(ELambda _ _ b) = do
        res <- checkReturn (BStmt b)
        unless res $ throwMsg
            ["Missing return value in lambda expression:\n", printTree e]
    checkExprReturn (EApp _ es) = mapM_ checkExprReturn es
    checkExprReturn _           = return ()

    checkReturn :: Stmt -> TCM Bool
    checkReturn (Ret _)                  = return True
    checkReturn VRet                     = return True
    checkReturn (CondElse _ s1 s2) = (&&) <$> checkReturn s1 <*> checkReturn s2
    checkReturn fndef@(FnDef _ _ _ b   ) = do
        res <- checkReturn (BStmt b)
        if res
            then return False
            else throwMsg
                ["Missing return value in function:\n", printTree fndef]
    checkReturn (SExp e) = do
        checkExprReturn e
        return False
    checkReturn (Ass _ e) = do
        checkExprReturn e
        return False
    checkReturn (Decl _ ds) = do
        mapM_ itemCheck ds
        return False
      where
        itemCheck (Init _ e) = checkExprReturn e
        itemCheck _          = return ()
    checkReturn (BStmt (Block ss)) = foldM checkOr False ss
        where checkOr acc s = (||) acc <$> checkReturn s
    checkReturn _ = return False

runTypeChecker (Program prog) = runReaderT (go prog)
    $ TCEnv M.empty 0 Nothing False
  where
    go prog = do
        checkStmtsM prog
        checkReturns prog
        ask
