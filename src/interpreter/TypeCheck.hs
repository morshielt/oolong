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

import           Data.List                      ( intercalate )
import           Data.Map                      as M
                                         hiding ( map
                                                , showTree
                                                )

-- TODO:TODO:TODO:TODO:TODO:TODO: print Type's (Int, Str, Fun [..]) as 'int', 'string' etc.!


show' :: Show a => [a] -> String
show' = intercalate ", " . map show

type Scope = Integer

type NameToTypeScope = M.Map Ident (TCType, Scope)

data TCEnv = TCEnv
  { nameToTypeScope :: NameToTypeScope
  , scope :: Scope
  , expectedRet :: Maybe TCType
  , inLoop :: Bool
  } deriving Show

type TCM a = ReaderT TCEnv (ExceptT String IO) a

data TCType = TString | TInt | TBool | TVoid | TFun [TCArg] TCType deriving Eq
data TCArg = R TCType | V TCType deriving Eq

instance Show TCType where
    show TString         = "`string`"
    show TInt            = "`int`"
    show TBool           = "`bool`"
    show TVoid           = "`void`"
    show (TFun args ret) = "`<(" ++ show' args ++ ") :" ++ show ret ++ ">`"

typeToTCType :: Type -> TCType
typeToTCType Str            = TString
typeToTCType Int            = TInt
typeToTCType Bool           = TBool
typeToTCType Void           = TVoid
typeToTCType (Fun args ret) = TFun (map valRefToTCArg args) (typeToTCType ret)

argToTCArg :: Arg -> TCArg
argToTCArg (Arg    t _) = V (typeToTCType t)
argToTCArg (RefArg t _) = R (typeToTCType t)

valRefToTCArg :: ByValOrRef -> TCArg
valRefToTCArg (ByVal t) = V (typeToTCType t)
valRefToTCArg (ByRef t) = R (typeToTCType t)

instance Show TCArg where
    show (R t) = show t ++ "&"
    show (V t) = show t

throwTCM :: String -> TCM a
throwTCM = lift . throwE

expectedButGotIn :: [TCType] -> TCType -> Expr -> TCM ()
expectedButGotIn [eT] gT e =
    when (eT /= gT)
        $  throwTCM
        $  "Expected type "
        ++ show eT
        ++ ", but got "
        ++ show gT
        ++ " in expression:\t"
        ++ printTree e

expectedButGotIn eTs gT e =
    when (gT `notElem` eTs)
        $  throwTCM
        $  "Expected one of types "
        ++ show' eTs
        ++ ", but got "
        ++ show gT
        ++ " in expression:\t"
        ++ printTree e

matchType :: TCType -> TCType -> TCM ()
matchType eT gT =
    when (eT /= gT)
        $  throwTCM
        $  "Expected type "
        ++ show eT
        ++ ", but got "
        ++ show gT

matchExpType :: TCType -> Expr -> TCM TCType
matchExpType t e = do
    eT <- checkExprM e
    matchType t eT
    return t

matchBinOpTypes :: [TCType] -> Expr -> Expr -> TCM TCType
matchBinOpTypes [t] e1 e2 = do
    e1T <- checkExprM e1
    e2T <- checkExprM e2
    matchType t e1T
    matchType t e2T
    return t

matchBinOpTypes ts e1 e2 = do
    e1T <- checkExprM e1
    expectedButGotIn ts e1T e1
    -- if e1T `elem` ts
        -- then do
    e2T <- checkExprM e2
    matchType e1T e2T
    return e1T
        -- else throwTCM "TODO matchBinOpTypes msg"

checkExprM :: Expr -> TCM TCType
checkExprM (ELambda args ret bs@(Block ss)) = do
    let ret' = typeToTCType ret
    -- scope <- asks scope
    -- env   <- asks nameToTypeScope
    tcEnv <- ask
    let t = TFun (argsToTypes args) ret'

    aVTS <- argsToVarAndTypeScope args
    let vTTS = nameToTypeScope tcEnv
    let newEnvWithArgs = tcEnv { nameToTypeScope = overwriteMap vTTS aVTS
                               , expectedRet     = Just ret'
                               }

    local (const newEnvWithArgs) $ checkStmtM (BStmt bs)
    return t

checkExprM (ELitInt _)        = return TInt
checkExprM ELitTrue           = return TBool
checkExprM ELitFalse          = return TBool
checkExprM (EString _       ) = return TString

checkExprM (Neg     e       ) = matchExpType TInt e
checkExprM (Not     e       ) = matchExpType TBool e

checkExprM (EMul e1 _     e2) = matchBinOpTypes [TInt] e1 e2
checkExprM (EAdd e1 Plus  e2) = matchBinOpTypes [TInt, TString] e1 e2
checkExprM (EAdd e1 Minus e2) = matchBinOpTypes [TInt] e1 e2

checkExprM (ERel e1 EQU   e2) = do
    matchBinOpTypes [TInt, TString, TBool] e1 e2
    return TBool
checkExprM (ERel e1 NE e2) = do
    matchBinOpTypes [TInt, TString, TBool] e1 e2
    return TBool
checkExprM (ERel e1 _ e2) = do
    matchBinOpTypes [TInt] e1 e2
    return TBool

checkExprM (EAnd e1 e2 ) = matchBinOpTypes [TBool] e1 e2
checkExprM (EOr  e1 e2 ) = matchBinOpTypes [TBool] e1 e2
checkExprM (EVar var   ) = getVarType var

checkExprM (EApp var es) = do
    typeScope <- getVarTypeScope var
    case typeScope of
        Nothing -> throwTCM $ "function" ++ show var ++ " is not declared"
        (Just (TFun args ret, s)) -> do
            -- eTs <- mapM checkExprM es
            checkArgs args es
            return ret
        (Just _) -> throwTCM $ show var ++ " is not a function"
  where
    checkArgs :: [TCArg] -> [Expr] -> TCM ()
    checkArgs args es = if length args == length es
        then mapM_ checkArg $ zip args es
        else throwTCM "Invalid number of arguments in function call"
    checkArg (R t, e@(EVar var)) = matchExpType t e
    checkArg (R t, _) = throwTCM "Reference argument must be a variable"
    checkArg (V t, e) = matchExpType t e


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
    when (t `notElem` [TString, TInt, TBool])
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
    -- let t' = typeToTCType t
    when (t == Void) $ throwTCM "Void variable declaration is forbidden"
    env <- ask
    foldM go env ds
  where
    go :: TCEnv -> Item -> TCM TCEnv
    go acc d = local (const acc) $ handleDecl (typeToTCType t) d

    handleDecl :: TCType -> Item -> TCM TCEnv
    handleDecl t d = do
        var <- case d of
            (DefaultInit var) -> case t of
                (TFun _ _) ->
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
    matchType TInt t
    ask

checkStmtM (Decr var) = do
    t <- getVarType var
    matchType TInt t
    ask

checkStmtM (While e s) = do
    matchExpType TBool e
    local (\env -> env { inLoop = True }) (checkStmtM s)
    ask

checkStmtM (Cond e s) = do
    matchExpType TBool e
    checkStmtM s
    ask

checkStmtM (CondElse e s1 s2) = do
    liftIO $ putStrLn "checkStmtM (CondElse e s1 s2)"
    matchExpType TBool e
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

checkStmtM VRet    = matchReturn TVoid

checkStmtM (Ret e) = do
    liftIO $ putStrLn "checkStmtM Ret"
    e' <- checkExprM e
    matchReturn e'



checkStmtM (FnDef ret name args bs@(Block ss)) = do
    let ret' = typeToTCType ret
    checkIfNameAlreadyInScope name

    scope <- asks scope
    env   <- asks nameToTypeScope
    tcEnv <- ask
    let t      = TFun (argsToTypes args) ret'
    let newEnv = tcEnv { nameToTypeScope = M.insert name (t, scope) env }

    aVTS <- argsToVarAndTypeScope args
    let vTTS = nameToTypeScope newEnv
    let newEnvWithArgs = newEnv { nameToTypeScope = overwriteMap vTTS aVTS
                                , expectedRet     = Just ret'
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

matchReturn :: TCType -> TCM TCEnv
matchReturn t = do
    ex <- asks expectedRet
    case ex of
        Nothing   -> throwTCM "Return outside of function"
        (Just eT) -> matchType eT t
    ask


argsToTypes :: [Arg] -> [TCArg]
argsToTypes = map argToTCArg
--   where
--     argToType :: Arg -> TCArg
--     argToType (Arg    t _) = ByVal t
--     argToType (RefArg t _) = ByRef t

argsToVarAndTypeScope :: [Arg] -> TCM NameToTypeScope
argsToVarAndTypeScope args = do
    scope <- asks scope
    let fScope = scope + 1
    let list = map
            (\arg -> case arg of
                (Arg    t var) -> (var, (typeToTCType t, fScope))
                (RefArg t var) -> (var, (typeToTCType t, fScope))
            )
            args
    let mapList = M.fromList list
    if length list == length mapList
        then return mapList
        else throwTCM "Function arguments must have different names"

getVarType :: Ident -> TCM TCType
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

getVarTypeScope :: Ident -> TCM (Maybe (TCType, Scope))
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

showExprType :: Expr -> TCType -> TCM () -- TODO: REMOVE EVERYWHERE, DEBUG ONLY
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

