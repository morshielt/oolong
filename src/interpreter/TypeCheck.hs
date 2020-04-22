module TypeCheck
    ( runTypeChecker
    )
where

import           AbsOolong
import           PrintOolong
import           ErrM

import           Types                          ( Var )
import           Utils                          ( overwriteMap )

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

type NameToTypeScope = M.Map Ident (Type, Scope)

data TCEnv = TCEnv
  { nameToTypeScope :: NameToTypeScope
  , scope :: Scope
  , expectedRet :: Maybe Type
  , inLoop :: Bool
  } deriving Show

type TCM a = ReaderT TCEnv (ExceptT String IO) a


throwTCM :: String -> TCM a
throwTCM = lift . throwE

expectedButGotIn :: Type -> Type -> Expr -> TCM a
expectedButGotIn eT gT e =
    throwTCM
        $  "Expected type "
        ++ show eT
        ++ ", but got "
        ++ show gT
        ++ " in expression:\n"
        ++ printTree e

matchType :: Type -> Type -> TCM ()
matchType eT gT =
    when (eT /= gT)
        $  throwTCM
        $  "Expected type "
        ++ show eT
        ++ ", but got "
        ++ show gT

matchExpType :: Type -> Expr -> TCM Type
matchExpType t e = do
    eT <- checkExprM e
    matchType t eT
    return t

matchBinOpTypes :: [Type] -> Expr -> Expr -> TCM Type
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

checkExprM :: Expr -> TCM Type
checkExprM (ELambda args ret bs@(Block ss)) = do
    -- scope <- asks scope
    -- env   <- asks nameToTypeScope
    tcEnv <- ask
    let t = Fun (argsToTypes args) ret

    aVTS <- argsToVarAndTypeScope args
    let vTTS = nameToTypeScope tcEnv
    let newEnvWithArgs = tcEnv { nameToTypeScope = overwriteMap vTTS aVTS
                               , expectedRet     = Just ret
                               }

    local (const newEnvWithArgs) $ checkStmtM (BStmt bs)
    return t


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
    typeScope <- getVarTypeScope var
    case typeScope of
        Nothing -> throwTCM $ "function" ++ show var ++ " is not declared"
        (Just (Fun args ret, s)) -> do
            -- eTs <- mapM checkExprM es
            checkArgs args es
            return ret
        (Just _) -> throwTCM $ show var ++ " is not a function"
  where
    checkArgs :: [ByValOrRef] -> [Expr] -> TCM ()
    checkArgs args es = if length args == length es
        then mapM_ checkArg $ zip args es
        else throwTCM "Invalid number of arguments in function call"
    checkArg (ByRef t, e@(EVar var)) = matchExpType t e
    checkArg (ByRef t, _) = throwTCM "Reference argument must be a variable"
    checkArg (ByVal t, e) = matchExpType t e


checkExprM e = do
    liftIO $ putStrLn $ printTree e
    error "checkExprM e"


checkStmtM :: Stmt -> TCM TCEnv
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
    env <- ask
    foldM go env ds
  where
    go :: TCEnv -> Item -> TCM TCEnv
    go acc d = local (const acc) $ handleDecl t d

    handleDecl :: Type -> Item -> TCM TCEnv
    handleDecl t d = do
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

        let val = (t, scope)
        env <- ask
        let nTTS = M.insert var val (nameToTypeScope env)
        return $ env { nameToTypeScope = nTTS }


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
    local (\env -> env { inLoop = True }) (checkStmtM s)
    ask

checkStmtM (Cond e s) = do
    matchExpType Bool e
    checkStmtM s
    ask

checkStmtM (CondElse e s1 s2) = do
    liftIO $ putStrLn "checkStmtM (CondElse e s1 s2)"
    matchExpType Bool e
    checkStmtM s1
    checkStmtM s2
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
    -- return env { expectedRet = expectedRet envAfter }
    ask

checkStmtM VRet    = matchReturn Void

checkStmtM (Ret e) = do
    liftIO $ putStrLn "checkStmtM Ret"
    e' <- checkExprM e
    matchReturn e'



checkStmtM (FnDef ret name args bs@(Block ss)) = do
    checkIfNameAlreadyInScope name

    scope <- asks scope
    env   <- asks nameToTypeScope
    tcEnv <- ask
    let t      = Fun (argsToTypes args) ret
    let newEnv = tcEnv { nameToTypeScope = M.insert name (t, scope) env }

    aVTS <- argsToVarAndTypeScope args
    let vTTS = nameToTypeScope newEnv
    let newEnvWithArgs = newEnv { nameToTypeScope = overwriteMap vTTS aVTS
                                , expectedRet     = Just ret
                                }

    envAfter <- local (const newEnvWithArgs) $ checkStmtM (BStmt bs)
    return newEnv
     -- o tutaj check myślęęęęęę, hmmmmmmmmm
    -- liftIO $ putStrLn "----------"
    -- liftIO $ putStrLn $ "Return type in FnDef: " ++ show (expectedRet envAfter)
    -- liftIO $ putStrLn $ printTree ss
    -- liftIO $ putStrLn "----------"

    -- --TODO: check return type!!!!!!!!!!!!!!!!!!!!!!!! and does every branch return
    -- case expectedRet envAfter of
    --     Nothing -> do
    --         liftIO $ putStrLn "Haven't checked function ret type."
    --         ask -- throwTCM "Missing return value TODO"
    --     (Just ret') -> if ret /= ret'
    --         then throwTCM "Invalid return type TODO"
    --         else return newEnv

checkStmtM Break = do
    loop <- asks inLoop
    if loop then ask else throwTCM "Break outside of a loop"

checkStmtM Continue = do
    loop <- asks inLoop
    if loop then ask else throwTCM "Continue outside of a loop"

checkStmtM e = do
    liftIO $ putStrLn $ printTree e
    error "checkStmtM e"

matchReturn :: Type -> TCM TCEnv
matchReturn t = do
    ex <- asks expectedRet
    case ex of
        Nothing   -> throwTCM "Return outside of function"
        (Just eT) -> matchType eT t
    ask


argsToTypes :: [Arg] -> [ByValOrRef]
argsToTypes = map argToType
  where
    argToType :: Arg -> ByValOrRef
    argToType (Arg    t _) = ByVal t
    argToType (RefArg t _) = ByRef t

argsToVarAndTypeScope :: [Arg] -> TCM NameToTypeScope
argsToVarAndTypeScope args = do
    scope <- asks scope
    let fScope = scope + 1
    let list = map
            (\arg -> case arg of
                (Arg    t var) -> (var, (t, fScope))
                (RefArg t var) -> (var, (t, fScope))
            )
            args
    let mapList = M.fromList list
    if length list == length mapList
        then return mapList
        else throwTCM "Function arguments must have different names"

getVarType :: Ident -> TCM Type
getVarType var = do
    typeScope <- getVarTypeScope var
    case typeScope of
        Nothing       -> throwTCM $ "Variable " ++ show var ++ " not declared"
        (Just (t, s)) -> return t

getVarScope :: Ident -> TCM Scope
getVarScope var = do
    typeScope <- getVarTypeScope var
    case typeScope of
        Nothing       -> throwTCM $ "Variable " ++ show var ++ " not declared"
        (Just (t, s)) -> return s

getVarTypeScope :: Ident -> TCM (Maybe (Type, Scope))
getVarTypeScope var = do
    scope <- asks scope
    env   <- asks nameToTypeScope
    return $ M.lookup var env

checkIfNameAlreadyInScope :: Ident -> TCM ()
checkIfNameAlreadyInScope var = do
    scope     <- asks scope
    typeScope <- getVarTypeScope var
    case typeScope of
        Nothing -> return ()
        (Just (_, s)) ->
            when (scope == s)
                $  throwTCM
                $  "Variable "
                ++ show var
                ++ " already declared"

showExprType :: Expr -> Type -> TCM () -- TODO: REMOVE EVERYWHERE, DEBUG ONLY
showExprType e t = liftIO $ putStrLn $ printTree e ++ " :: " ++ show t

checkStmtsM :: [Stmt] -> TCM TCEnv
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


-- runTypeChecker (Program prog) = return ()
runTypeChecker (Program prog) = runReaderT (go prog)
    $ TCEnv M.empty 0 Nothing False
  where
    go prog = do
        checkStmtsM prog
        checkReturns prog
        ask



checkReturns :: [Stmt] -> TCM ()
checkReturns = mapM_ checkReturn
  where
    checkExprReturn :: Expr -> TCM ()
    checkExprReturn e@(ELambda _ _ b) = do
        res <- checkReturn (BStmt b)
        unless res $ do
            liftIO $ putStrLn $ printTree e
            throwTCM "Missing return value"
    checkExprReturn (EApp _ es) = mapM_ checkExprReturn es
    checkExprReturn _           = return ()

    checkReturn :: Stmt -> TCM Bool
    checkReturn (Ret _)            = return True
    checkReturn VRet               = return True
    checkReturn (CondElse _ s1 s2) = (&&) <$> checkReturn s1 <*> checkReturn s2
    checkReturn (FnDef _ _ _ b   ) = do
        res <- checkReturn (BStmt b)
        if res
            then return False
            else do
                liftIO $ putStrLn $ printTree (BStmt b)
                throwTCM "Missing return value"
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
        -- where checkOr acc s = checkReturn s >>= (\b -> return $ acc || b)
        where checkOr acc s = (||) acc <$> checkReturn s
    --   where
    --     checkOr acc s = do
    --         res <- mapM checkReturn ss
    --         return $ or res
    checkReturn _ = return False

