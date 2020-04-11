module Types where

import           Data.Map                      as M

import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Except

type Var = String
type Loc = Integer

type VarToLoc = M.Map Var Loc -- ENV

-- type Fun = 

data Val = VInt Integer | VBool Bool | VString String | VVoid deriving (Show)  -- | VFun Fun 

type LocToVal = M.Map Loc Val -- STORE

-- data RuntimeException = DivisionByZeroException | ModulusByZeroException | NoReturnException deriving Show
data RuntimeException = DivisionByZeroException | ModulusByZeroException deriving Show

type ReturnResult = Maybe Val

data IMState = IMState
  { locToVal :: LocToVal
  , freeLoc :: Loc
  } deriving Show


type IMon a = ReaderT VarToLoc (StateT IMState (ExceptT RuntimeException IO)) a


embedded :: ReaderT VarToLoc (StateT IMState (ExceptT RuntimeException IO)) Int
embedded = do
    -- throwError DivisionByZeroException -- Left DivisionByZeroException
    return 1 -- Right (1,IMState {locToVal = fromList [], freeLoc = 0})

readerUnwrap :: VarToLoc -> (StateT IMState (ExceptT RuntimeException IO)) Int
readerUnwrap = runReaderT embedded

stateUnwrap :: IMState -> ExceptT RuntimeException IO (Int, IMState)
stateUnwrap = runStateT $ readerUnwrap M.empty

exceptUnwrap :: IO (Either RuntimeException (Int, IMState))
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
--     runExceptT $ runStateT (runReaderT (interpretMany prog) M.empty) $ IMState
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
