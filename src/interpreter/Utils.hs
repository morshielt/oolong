module Utils where

import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Except


import           Data.Map                      as M
import           Types

-- INTERP ONLY

overwriteMap :: Ord a => M.Map a b -> M.Map a b -> M.Map a b
overwriteMap = M.unionWith (curry snd)

throwM :: String -> IM a
throwM = lift . lift . throwE

alloc :: IM Loc
alloc = do
    modify (\st -> st { freeLoc = freeLoc st + 1 })
    gets freeLoc

allocAndPutVal :: Val -> IM ()
allocAndPutVal val = do
    loc <- alloc
    putVal loc val

getLoc :: Var -> IM Loc
getLoc var = do
    env <- ask
    let loc = M.lookup var env
    case loc of
        Nothing ->
            throwM $ "getLoc: Variable " ++ var ++ " location not found!!!"
        (Just loc') -> return loc'

getVal :: Loc -> IM Val
getVal loc = do
    store <- gets locToVal
    let val = M.lookup loc store
    case val of
        Nothing ->
            throwM $ "getVal: Location's " ++ show loc ++ "value not found!!!"
        (Just val') -> return val'


putVal :: Loc -> Val -> IM ()
putVal loc val = do
    modify
        (\st ->
            let mod = M.insert loc val (locToVal st) in st { locToVal = mod }
        )
    return ()

changeVal :: Var -> Val -> IM ()
changeVal var val = do
    loc <- getLoc var
    putVal loc val

readVar :: Var -> IM Val
readVar var = do
    loc <- getLoc var
    getVal loc

setLoc :: Var -> Loc -> IM Env
setLoc var loc = asks (M.insert var loc)

declare :: Var -> Val -> IM Env
declare var val = do
    loc <- alloc
    putVal loc val
    setLoc var loc

-- declFn name  = do
--     env <- ask
--     loc <- alloc
--     let env' = M.insert name loc env
--     let val' = VFun args ret env' ss
--     putVal loc val'

