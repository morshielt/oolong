module TCUtils where


import           AbsOolong

import           TCTypes

import           Control.Monad.Reader
import           Control.Monad.Trans.Except
import           Control.Monad.Except

import           Data.List                      ( intercalate )
import           Data.Map                      as M
                                         hiding ( map )

getVarType :: Var -> TCM TCType
getVarType var = do
    typeScope <- getVarTypeScope var
    case typeScope of
        Nothing       -> throwTCM $ unwords ["Variable ", var, " not declared"]
        (Just (t, s)) -> return t

getVarTypeScope :: Var -> TCM (Maybe (TCType, Scope))
getVarTypeScope var = do
    scope <- asks scope
    env   <- asks types
    return $ M.lookup var env

checkIfNameAlreadyInScope :: Var -> TCM ()
checkIfNameAlreadyInScope var = do
    scope     <- asks scope
    typeScope <- getVarTypeScope var
    case typeScope of
        Nothing -> return ()
        (Just (_, s)) ->
            when (scope == s)
                $  throwTCM
                $  "Variable "
                ++ var
                ++ " already declared"

throwTCM :: String -> TCM a
throwTCM = lift . throwE

throwExtraMsg :: TCM a -> (String -> [String]) -> TCM a
throwExtraMsg act msg = catchError act (throwTCM . unwords . msg)

throwMsg :: [String] -> TCM a
throwMsg = throwTCM . unwords
