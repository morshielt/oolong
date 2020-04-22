module Utils where

import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Except


import           Data.Map                      as M
import           Types

-- INTERP ONLY

overwriteMap :: Ord a => M.Map a b -> M.Map a b -> M.Map a b
overwriteMap = M.unionWith (curry snd)

throwM :: String -> IMon a
throwM = lift . lift . throwE

alloc :: IMon Loc
alloc = do
    modify (\st -> st { freeLoc = freeLoc st + 1 })
    gets freeLoc

allocAndPutVal :: Val -> IMon ()
allocAndPutVal val = do
    loc <- alloc
    putVal loc val

getLoc :: Var -> IMon Loc
getLoc var = do
    env <- ask
    let loc = M.lookup var env
    case loc of
        Nothing ->
            throwM $ "getLoc: Variable " ++ var ++ " location not found!!!"
        (Just loc') -> return loc'

getVal :: Loc -> IMon Val
getVal loc = do
    store <- gets locToVal
    let val = M.lookup loc store
    case val of
        Nothing ->
            throwM $ "getVal: Location's " ++ show loc ++ "value not found!!!"
        (Just val') -> return val'


putVal :: Loc -> Val -> IMon ()
putVal loc val = do
    modify
        (\st ->
            let mod = M.insert loc val (locToVal st) in st { locToVal = mod }
        )
    return ()

changeVal :: Var -> Val -> IMon ()
changeVal var val = do
    loc <- getLoc var
    putVal loc val

readVar :: Var -> IMon Val
readVar var = do
    loc <- getLoc var
    getVal loc

setLoc :: Var -> Loc -> IMon Env
setLoc var loc = asks (M.insert var loc)

declare :: Var -> Val -> IMon Env
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

