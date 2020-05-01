module Types where

import           Data.Map                      as M
                                         hiding ( map )

import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Except

import           AbsOolong

-- Main interpreter monad
type IM a = ReaderT Env (StateT IMState (ExceptT String IO)) a

-- Env
type Var = String
type Loc = Integer
type Env = M.Map Var Loc

-- Store
type Store = M.Map Loc Val
data IMState = IMState
  { locToVal :: Store
  , freeLoc :: Loc
  }

-- Value
data Val = VInt Integer
         | VBool Bool
         | VString String
         | VVoid
         | VFun [Arg] Type Env [Stmt]

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

defaultVal :: Type -> Val
defaultVal Int  = VInt 0
defaultVal Bool = VBool False
defaultVal Str  = VString ""

-- State of computations
    -- Nothing - normal
    -- Br - `break` signal ocurred
    -- Cont - `continue` signal ocurred
    -- R value - value was returned 
type Flow = Maybe FlowVal
data FlowVal = Br | Cont | R Val
