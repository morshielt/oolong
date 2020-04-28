module Types where

import           Data.Map                      as M
                                         hiding ( map )

import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Except

import           AbsOolong

type Var = String

type Loc = Integer

type Env = M.Map Var Loc

-- valToType :: Val -> Type
-- valToType (VInt    _)         = Int
-- valToType (VBool   _)         = Bool
-- valToType (VString _)         = Str
-- valToType VVoid               = Void
-- valToType (VFun args ret _ _) = Fun (map fst args') ret
--   where
--     args' = map argToType args
--     argToType (Arg    t (Ident var)) = (ByVal t, var)
--     argToType (RefArg t (Ident var)) = (ByRef t, var)

                                --   args       ret   env(closure)   body
                                --TODO: jaki ten VFun w końcu?
data Val = VInt Integer | VBool Bool | VString String | VVoid | VFn ([Expr] -> IM Val) | VFun [Arg]  Type  Env     [Stmt] -- deriving (Show)
-- data Val = VInt Integer | VBool Bool | VString String | VVoid | VFun [Type] Type ([Val] -> IM(Env, ReturnVal))

defaultVal :: Type -> Val
defaultVal Int  = VInt 0
defaultVal Bool = VBool False
defaultVal Str  = VString ""

instance Show Val where
    show v = case v of
        VInt    i     -> show i
        VBool   True  -> "true"
        VBool   False -> "false"
        VString s     -> s

instance Eq Val where
    (VInt    v) == (VInt    v') = v == v'
    (VBool   v) == (VBool   v') = v == v'
    (VString v) == (VString v') = v == v'
    _           == _            = False

type Flow = Maybe FlowVal
data FlowVal = Br | Cont | R Val

type Store = M.Map Loc Val -- STORE

data IMState = IMState
  { locToVal :: Store
  , freeLoc :: Loc
  } deriving Show

type IM a = ReaderT Env (StateT IMState (ExceptT String IO)) a
