module Utils where

import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Except


import           Data.Map                      as M
import           Types

throwM :: String -> IMon a
throwM = lift . lift . throwE

alloc :: Val -> IMon Loc
alloc val = do
    modify (\st -> st { freeLoc = freeLoc st + 1 })
    gets freeLoc

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


putNewVal :: Loc -> Val -> IMon ()
putNewVal loc val = do
    modify
        (\st ->
            let mod = M.insert loc val (locToVal st) in st { locToVal = mod }
        )
    return ()

putVal :: Loc -> Val -> IMon ()
putVal loc val = do
    old <- getVal loc
    if valToType old == valToType val
        then putNewVal loc val
        else
            throwM
            $  "putVal: Expected type: "
            ++ show (valToType old)
            ++ " Got: "
            ++ show (valToType val)

changeVal :: Var -> Val -> IMon ()
changeVal var val = do
    loc <- getLoc var
    putVal loc val

readVal :: Var -> IMon Val
readVal var = do
    loc <- getLoc var
    getVal loc
