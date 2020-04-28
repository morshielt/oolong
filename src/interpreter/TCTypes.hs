module TCTypes where

import           AbsOolong

import           Types                          ( Var )

import           Control.Monad.Reader
import           Control.Monad.Trans.Except

import           Data.List                      ( intercalate )
import           Data.Map                      as M
                                         hiding ( map )

type Scope = Integer

type Types = M.Map Var (TCType, Scope)

data TCEnv = TCEnv
  { types :: Types
  , scope :: Scope
  , expectedRet :: Maybe (TCType, Var)
  , inLoop :: Bool
  } deriving Show

type TCM a = ReaderT TCEnv (ExceptT String IO) a

show' :: Show a => [a] -> String
show' = intercalate ", " . map show

data TCType = TString | TInt | TBool | TVoid | TFun [TCArg] TCType deriving Eq
data TCArg = R TCType | V TCType deriving Eq

instance Show TCType where
    show TString         = "string"
    show TInt            = "int"
    show TBool           = "bool"
    show TVoid           = "void"
    show (TFun args ret) = "<(" ++ show' args ++ ") : " ++ show ret ++ ">"

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
