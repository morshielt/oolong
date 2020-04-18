module TypeCheck
    ( runTypeChecker
    )
where

import           AbsOolong
import           PrintOolong
import           ErrM

import           Types                          ( Var )

import           Control.Monad                  ( when )
import           Control.Monad.Reader
import           Control.Monad.Except
import           Control.Monad.Trans.Except

import           Data.Map                      as M
                                         hiding ( map
                                                , showTree
                                                )

-- TODO:TODO:TODO:TODO:TODO:TODO: print Type's (Int, Str, Fun [..]) as 'int', 'string' etc.!

type Scope = Integer

data TypeOrRef t = T t | R t deriving Show -- Type or Reference

type ArgType = TypeOrRef Type

data FunType = F Type [ArgType]

instance Show FunType where
    show (F t args) = "<" ++ show args ++ ">" ++ " : " ++ show t

data TCEnv = TCEnv
  { varToTypeScope :: M.Map Ident (Type, Scope)
  , funToTypeScope :: M.Map Ident (FunType, Scope)
  , scope :: Scope
  , expectedRet :: Maybe Type
  } deriving Show

type TCMon a = ReaderT TCEnv (ExceptT String IO) a


throwTCM :: String -> TCMon a
throwTCM = lift . throwE

expectedButGotIn :: Type -> Type -> Expr -> TCMon a
expectedButGotIn eT gT e =
    throwTCM
        $  "Expected type "
        ++ show eT
        ++ ", but got "
        ++ show gT
        ++ " in expression:\n"
        ++ printTree e

matchType :: Type -> Type -> TCMon ()
matchType eT gT =
    when (eT /= gT)
        $  throwTCM
        $  "Expected type "
        ++ show eT
        ++ ", but got "
        ++ show gT

matchExpType :: Type -> Expr -> TCMon Type
matchExpType t e = do
    eT <- checkExprM e
    matchType t eT
    return t

matchBinOpTypes :: [Type] -> Expr -> Expr -> TCMon Type
matchBinOpTypes [t] e1 e2 = do
    e1T <- checkExprM e1
    e2T <- checkExprM e2
    matchType t e1T
    matchType t e2T
    return t
matchBinOpTypes ts e1 e2 = do
    e1T <- checkExprM e1
    if e1T `elem` ts
        then do
            e2T <- checkExprM e2
            matchType e1T e2T
            return e1T
        else throwTCM "TODO matchBinOpTypes msg"

checkExprM :: Expr -> TCMon Type
checkExprM (ELitInt _)        = return Int
checkExprM ELitTrue           = return Bool
checkExprM ELitFalse          = return Bool
checkExprM (EString _       ) = return Str

checkExprM (Neg     e       ) = matchExpType Int e
checkExprM (Not     e       ) = matchExpType Bool e

checkExprM (EMul e1 _     e2) = matchBinOpTypes [Int] e1 e2
checkExprM (EAdd e1 Plus  e2) = matchBinOpTypes [Int, Str] e1 e2
checkExprM (EAdd e1 Minus e2) = matchBinOpTypes [Int] e1 e2

checkExprM (ERel e1 EQU   e2) = do
    matchBinOpTypes [Int, Str, Bool] e1 e2
    return Bool
checkExprM (ERel e1 NE e2) = do
    matchBinOpTypes [Int, Str, Bool] e1 e2
    return Bool
checkExprM (ERel e1 _ e2) = do
    matchBinOpTypes [Int] e1 e2
    return Bool

checkExprM (EAnd e1 e2 ) = matchBinOpTypes [Bool] e1 e2
checkExprM (EOr  e1 e2 ) = matchBinOpTypes [Bool] e1 e2
checkExprM (EVar var   ) = getVarType var

checkExprM (EApp var es) = do
    typeScope <- getFunTypeScope var
    case typeScope of
        Nothing                -> throwTCM $ show var ++ " is not a function"
        (Just (F ret args, s)) -> do
            -- eTs <- mapM checkExprM es
            checkArgs args es
            return ret
  where
    checkArgs :: [ArgType] -> [Expr] -> TCMon ()
    checkArgs args es = mapM_ checkArg $ zip args es
    checkArg (R t, e@(EVar var)) = matchExpType t e
    checkArg (R t, _) = throwTCM "Reference argument must be a variable"
    checkArg (T t, e) = matchExpType t e


checkExprM e = do
    liftIO $ putStrLn $ printTree e
    error "XD"


checkStmtM :: Stmt -> TCMon TCEnv
checkStmtM Empty    = ask

checkStmtM (SExp e) = do
    t <- checkExprM e
    showExprType e t
    ask

checkStmtM (SPrint e) = do
    t <- checkExprM e
    when (t `notElem` [Str, Int, Bool])
        $  throwTCM
        $  "Cannot print type "
        ++ show t
    showExprType e t
    ask

checkStmtM (Ass var e) = do
    t <- getVarType var
    matchExpType t e
    showExprType e t
    ask

checkStmtM (Decl t ds) = do
    when (t == Void) $ throwTCM "Void variable declaration is forbidden"
    ds' <- mapM (declToPair t) ds
    let decls = M.fromList ds'
    env <- asks varToTypeScope
    -- FIXME:FIXED: NIE można zrobić guard na intersection xD
    tcEnv <- ask
    return $ tcEnv { varToTypeScope = M.unionWith (curry snd) env decls } -- TODO: KONIECZNIE SND
  where
    declToPair :: Type -> Item -> TCMon (Ident, (Type, Scope))
    declToPair t d = do
        var <- case d of
            (DefaultInit var) -> case t of
                (Fun _ _) ->
                    throwTCM "Default function declaration is forbidden"
                _ -> return var
            (Init var e) -> do
                matchExpType t e
                showExprType e t
                return var
        checkIfNameAlreadyInScope var
        scope <- asks scope
        return (var, (t, scope))

checkStmtM (Incr var) = do
    t <- getVarType var
    matchType Int t
    ask

checkStmtM (Decr var) = do
    t <- getVarType var
    matchType Int t
    ask

checkStmtM (While e s) = do
    matchExpType Bool e
    checkStmtM s
    ask

checkStmtM (Cond e s) = do
    matchExpType Bool e
    checkStmtM s
    ask

checkStmtM (CondElse e s1 s2) = do
    liftIO $ putStrLn "checkStmtM (CondElse e s1 s2)"
    matchExpType Bool e
    env1 <- checkStmtM s1
    env2 <- checkStmtM s2
    let ret1 = expectedRet env1
    let ret2 = expectedRet env2
    case (ret1, ret2) of
        (Nothing, Nothing) -> ask
        (Nothing, Just _) ->
            throwTCM "Missing return value in one of if/else branches"
        (Just _, Nothing) ->
            throwTCM "Missing return value in one of if/else branches"
        (Just t1, Just t2) -> do
            matchType t1 t2 -- Message might be dumb. "Expected type Str, but got Int", when we were in int type function.
            ask

checkStmtM (BStmt (Block ss)) = do
    liftIO $ putStrLn "checkStmtM (BStmt (Block ss))"
    env      <- ask
    s        <- asks scope
    envAfter <- local (\env -> env { scope = s + 1 }) (checkStmtsM ss)
    liftIO $ putStrLn "----------"
    liftIO $ putStrLn $ "Return type: " ++ show (expectedRet envAfter)
    liftIO $ putStrLn $ printTree ss
    liftIO $ putStrLn "----------"
    return env { expectedRet = expectedRet envAfter }
    -- ask

checkStmtM VRet    = matchReturn Void

checkStmtM (Ret e) = do
    liftIO $ putStrLn "checkStmtM Ret"
    e' <- checkExprM e
    matchReturn e'



checkStmtM (FnDef ret name args bs@(Block ss)) = do
    checkIfNameAlreadyInScope name

    scope <- asks scope
    env   <- asks funToTypeScope
    tcEnv <- ask
    let t      = F ret (argsToTypes args)
    let newEnv = tcEnv { funToTypeScope = M.insert name (t, scope) env }

    aVTS <- argsToVarAndTypeScope args
    let vTTS = varToTypeScope newEnv
    let newEnvWithArgs = newEnv
            { varToTypeScope = M.unionWith (curry snd) vTTS (M.fromList aVTS)
            , expectedRet    = Just ret
            }

    envAfter <- local (const newEnvWithArgs) $ checkStmtM (BStmt bs)
     -- o tutaj check myślęęęęęę, hmmmmmmmmm
    liftIO $ putStrLn "----------"
    liftIO $ putStrLn $ "Return type in FnDef: " ++ show (expectedRet envAfter)
    liftIO $ putStrLn $ printTree ss
    liftIO $ putStrLn "----------"

    -- --TODO: check return type!!!!!!!!!!!!!!!!!!!!!!!! and does every branch return
    case expectedRet envAfter of
        Nothing -> do
            liftIO $ putStrLn "Haven't checked function ret type."
            ask -- throwTCM "Missing return value TODO"
        (Just ret') -> if ret /= ret'
            then throwTCM "Invalid return type TODO"
            else return newEnv



matchReturn :: Type -> TCMon TCEnv
matchReturn t = do
    ex <- asks expectedRet
    case ex of
        Nothing   -> throwTCM "Return outside of function"
        (Just eT) -> matchType eT t
    ask


argsToTypes :: [Arg] -> [ArgType]
argsToTypes = map argToType
  where
    argToType :: Arg -> ArgType
    argToType (Arg    t _) = T t
    argToType (RefArg t _) = R t

argsToVarAndTypeScope :: [Arg] -> TCMon [(Ident, (Type, Scope))]
argsToVarAndTypeScope args = do
    scope <- asks scope
    let fScope = scope + 1
    return $ map
        (\arg -> case arg of
            (Arg    t var) -> (var, (t, fScope))
            (RefArg t var) -> (var, (t, fScope))
        )
        args

getVarType :: Ident -> TCMon Type
getVarType var = do
    typeScope <- getVarTypeScope var
    case typeScope of
        Nothing       -> throwTCM $ "Variable " ++ show var ++ " not declared"
        (Just (t, s)) -> return t

getVarScope :: Ident -> TCMon Scope
getVarScope var = do
    typeScope <- getVarTypeScope var
    case typeScope of
        Nothing       -> throwTCM $ "Variable " ++ show var ++ " not declared"
        (Just (t, s)) -> return s

getVarTypeScope :: Ident -> TCMon (Maybe (Type, Scope))
getVarTypeScope var = do
    scope <- asks scope
    env   <- asks varToTypeScope
    return $ M.lookup var env

getFunTypeScope :: Ident -> TCMon (Maybe (FunType, Scope))
getFunTypeScope var = do
    scope <- asks scope
    env   <- asks funToTypeScope
    return $ M.lookup var env

checkIfNameAlreadyInScope :: Ident -> TCMon ()
checkIfNameAlreadyInScope var = do
    scope      <- asks scope
    typeScope  <- getVarTypeScope var
    ftypeScope <- getFunTypeScope var
    case typeScope of
        Nothing -> return ()
        (Just (_, s)) ->
            when (scope == s)
                $  throwTCM
                $  "Variable "
                ++ show var
                ++ " already declared"
    case ftypeScope of
        Nothing -> return ()
        (Just (_, s)) ->
            when (scope == s)
                $  throwTCM
                $  "Function "
                ++ show var
                ++ " already declared"

showExprType :: Expr -> Type -> TCMon () -- TODO: REMOVE EVERYWHERE, DEBUG ONLY
showExprType e t = liftIO $ putStrLn $ printTree e ++ " :: " ++ show t

checkStmtsM :: [Stmt] -> TCMon TCEnv
checkStmtsM []       = ask
checkStmtsM (s : xs) = do
    env <- checkStmtM s
    local (const env) (checkStmtsM xs)
    -- let ret = expectedRet env
    -- case ret of
    --     Nothing ->
    --         -- liftIO $ putStrLn "NOTHINGrETURNED~  "
    --         local (const env) (checkStmtsM xs) -- always run in 'new' env, which is only sometimes changed
    --     _ -> do
    --         liftIO $ putStrLn "Type's been returned~  "
    --         return env


runTypeChecker (Program prog) =
    runReaderT (checkStmtsM prog) $ TCEnv M.empty M.empty 0 Nothing
-- runTypeChecker (Program prog) = return ()
