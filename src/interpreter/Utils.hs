module Utils where

import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Except


import           Data.Map                      as M
import           Types

-- INTERP ONLY

setBreak :: Val -> IMon ()
setBreak val = modify
    (\st ->
        let mod = M.insert breakLoc val (locToVal st) in st { locToVal = mod }
    )

setContinue :: Val -> IMon ()
setContinue val = modify
    (\st ->
        let mod = M.insert continueLoc val (locToVal st)
        in  st { locToVal = mod }
    )


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

replaceVal :: Loc -> Val -> IMon ()
replaceVal loc val = do
    old <- getVal loc
    if valToType old == valToType val
        then putNewVal loc val
        else
            throwM
            $  "replaceVal: Expected type: "
            ++ show (valToType old)
            ++ " Got: "
            ++ show (valToType val)

changeVal :: Var -> Val -> IMon ()
changeVal var val = do
    loc <- getLoc var
    replaceVal loc val

readVal :: Var -> IMon Val
readVal var = do
    loc <- getLoc var
    getVal loc
