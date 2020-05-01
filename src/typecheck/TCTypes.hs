module TCTypes where

import           AbsOolong

import           Control.Monad.Reader
import           Control.Monad.Trans.Except

import           Data.List                      ( intercalate )
import           Data.Map                      (Map)

-- Main type check monad
type TCM a = ReaderT TCEnv (ExceptT String IO) a

-- Type checker env
type Var = String
type Scope = Integer
type Types = Map Var (TCType, Scope)

data TCEnv = TCEnv
  { types :: Types
  , scope :: Scope -- current scope
  , expectedRet :: Maybe (TCType, Var) -- expected return type and expecting function name
  , inLoop :: Bool
  } deriving Show

-- Custom type 
data TCType = TString | TInt | TBool | TVoid | TFun [TCArg] TCType deriving Eq
data TCArg = R TCType | V TCType deriving Eq -- argument by reference or by value

instance Show TCArg where
    show (R t) = show t ++ "&"
    show (V t) = show t

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

valRefToTCArg :: ByValOrRef -> TCArg
valRefToTCArg (ByVal t) = V (typeToTCType t)

valRefToTCArg (ByRef t) = R (typeToTCType t)
argToTCArg :: Arg -> TCArg
argToTCArg (Arg    t _) = V (typeToTCType t)
argToTCArg (RefArg t _) = R (typeToTCType t)

show' :: Show a => [a] -> String
show' = intercalate ", " . map show



