module Types where

import           Data.Map                      as M

import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Except

import AbsOolong 



type Var = String
type Loc = Integer

type VarToLoc = M.Map Var Loc -- ENV

-- type Fun = 

valToType :: Val -> Type
valToType (VInt    _) = Int
valToType (VBool   _) = Bool
valToType (VString _) = Str
valToType VVoid       = Void
valToType _ = error "valToType unimplemented yet"

data Val = VInt Integer | VBool Bool | VString String | VVoid deriving (Show)  -- | VFun Fun 

type LocToVal = M.Map Loc Val -- STORE

type ReturnVal = Maybe Val

data IMState = IMState
  { locToVal :: LocToVal
  , freeLoc :: Loc
  } deriving Show


type IMon a = ReaderT VarToLoc (StateT IMState (ExceptT String IO)) a

-- type IExcept = ExceptT String IO
-- type IState = StateT Store IExcept
-- type Interpreter = ReaderT Env IState


embedded :: ReaderT VarToLoc (StateT IMState (ExceptT String IO)) Int
embedded = do
    throwError "DivisionByZeroException" -- Left DivisionByZeroException
    return 1 -- Right (1,IMState {locToVal = fromList [], freeLoc = 0})

readerUnwrap :: VarToLoc -> (StateT IMState (ExceptT String IO)) Int
readerUnwrap = runReaderT embedded

stateUnwrap :: IMState -> ExceptT String IO (Int, IMState)
stateUnwrap = runStateT $ readerUnwrap M.empty

exceptUnwrap :: IO (Either String (Int, IMState))
exceptUnwrap = runExceptT $ stateUnwrap $ IMState M.empty 0


-- newtype IMon a = IMon (ReaderT VarToLoc (StateT IMState (ExceptT RuntimeException IO)) a)
--   deriving ( Functor
--            , Applicative
--            , Monad
--            , MonadReader VarToLoc
--            , MonadState IMState
--            , MonadError RuntimeException
--            , MonadIO
--            )


-- runInterpreter prog =
--     runExceptT $ runStateT (runReaderT (execStmtsM prog) M.empty) $ IMState
--         M.empty
--         0

-- runMyMonad :: VarToLoc -> IMState -> IMon a -> IO (Either RuntimeException a)
-- runMyMonad VarToLoc state (IMon m) = runExceptT s
--   where
--     r = runReaderT m VarToLoc
--     s = evalStateT r state


-- initialVarToLocironment :: VarToLoc
-- initialVarToLocironment = M.empty

-- initialState :: IMState
-- initialState = IMState M.empty 0
