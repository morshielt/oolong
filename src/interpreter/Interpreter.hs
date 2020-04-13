-- {-# LANGUAGE FlexibleContexts #-}
module Interpreter where

import           AbsOolong

import           Types
import           Utils
import           Expressions

import           Data.Map                      as M

import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Except

-- TODO: nigdzie się jeszcze nie sprawdza, 
-- czy w danym scope nie ma już czasem zmiennej o takiej nazwie!

continueInCurrentEnv :: IMon (VarToLoc, ReturnVal)
continueInCurrentEnv = do
    env <- ask
    return (env, Nothing)

valToString :: ReturnVal -> String
valToString res = case res of
    (Just val) -> case val of
        (VInt    x) -> show x
        (VBool   b) -> show b
        (VString s) -> s
        _           -> error "ERROR valToString"
    Nothing -> error "ERROR Nothing valToString"


execStmtM :: Stmt -> IMon (VarToLoc, ReturnVal)
execStmtM Empty      = continueInCurrentEnv

execStmtM (SPrint e) = do
    e' <- evalExprM e
    case e' of
        (Just val) -> if valToType val `notElem` [Int, Bool, Str]
            then throwM $ "print: invalid type of expression: " ++ show
                (valToType val)
            else do
                liftIO $ putStrLn $ case val of
                    (VInt    x) -> show x
                    (VBool   b) -> show b
                    (VString s) -> s
                continueInCurrentEnv
        Nothing -> throwM $ "print: invalid expression " ++ show e

execStmtM (Decl _ []) = throwM "Miracle, that should never get through parsing phase."
execStmtM (Decl Void _) =
    throwM $ "Illegal type for variable declaration: " ++ show Void
execStmtM (Decl (Fun _ _) (DefaultInit _ : _)) =
    throwM "Function declaration without initialisation is forbidden."

execStmtM (Decl t ds) = do
    ds' <- mapM declToPair ds
    let decls = M.fromList ds'
    env <- ask
    let env' = M.unionWith (curry snd) env decls -- TODO: musi być snd? 
    return (env', Nothing)
    -- w sensie teoretycznie chyba i tak nie powinno być zmiennych z tą samą nazwą 
    -- w tym samym bloku, ale wydaje mi się, że to się przyda to nadpisywania w kolejnych blokach,
    -- nie wiem
  where
    declToPair :: Item -> IMon (Var, Loc)
    declToPair d = do
        (var, val) <- unpackDeclaration d
        loc        <- alloc val
        return (var, loc)
    unpackDeclaration :: Item -> IMon (Var, Val)
    unpackDeclaration (DefaultInit (Ident var)) =
        (return . (,) var) $ case t of
            Int  -> VInt 0
            Bool -> VBool False
            Str  -> VString ""
    unpackDeclaration (Init (Ident var) e) = do
        val <- getValAndCheckType t e
        return (var, val)
    getValAndCheckType :: Type -> Expr -> IMon Val
    getValAndCheckType t e = do
        e' <- evalExprM e
        case e' of
            (Just val) -> if t == valToType val
                then return val
                else
                    throwM
                    $  "Expression \""
                    ++ show e
                    ++ "\" doesn't match type: "
                    ++ show t
            Nothing ->
                throwM
                    $  "Illegal expression for variable declaration: "
                    ++ show e

-- execStmtM (Decl _ []) = continueInCurrentEnv
-- execStmtM (Decl t (d : ds)) = do
--     (var, val) <- unpackDeclaration d
--     loc        <- alloc val
--     env        <- ask -- TODO: tu się powtarza to co niżej~
--     let env' = M.insert var loc env
--     local (const env') (execStmtM (Decl t ds))
--   where
--     unpackDeclaration :: Item -> IMon (Var, Val)
--     unpackDeclaration (DefaultInit (Ident var)) = (return . (,) var) $ case t of
--         Int  -> VInt 0
--         Bool -> VBool False
--         Str  -> VString ""
--     unpackDeclaration (Init (Ident var) e) = do
--         val <- getValAndCheckType t e
--         return (var, val)

--     getValAndCheckType :: Type -> Expr -> IMon Val
--     getValAndCheckType t e = do
--         e' <- evalExprM e
--         case e' of
--             (Just val) -> if t == valToType val
--                 then return val
--                 else
--                     throwM
--                     $  "Expression \""
--                     ++ show e
--                     ++ "\" doesn't match type: "
--                     ++ show t
--             Nothing ->
--                 throwM $ "Illegal expression for variable declaration: " ++ show e



execStmtM (Ass (Ident var) e) = do
    e' <- evalExprM e
    case e' of
        (Just val) -> do
            changeVal var val
            continueInCurrentEnv
        Nothing ->
            throwM $ "Illegal expression for variable assignment: " ++ show e

        -- let env' = M.insert var loc env
        -- local (const env') (execStmtM (Decl t items))


execStmtM (While e s) = execCondAndActM e continueWhile continueInCurrentEnv
  where
    continueWhile = do
        execStmtM s
        execStmtM (While e s)

execStmtM (Cond e s) = execCondAndActM e (execStmtM s) continueInCurrentEnv

execStmtM (CondElse e s1 s2) = execCondAndActM e (execStmtM s1) (execStmtM s2)

execStmtM (BStmt (Block ss)) = execStmtsM ss

execStmtM _                  = error "Not implemented yet or error xD"

execCondAndActM
    :: Expr
    -> IMon (VarToLoc, ReturnVal)
    -> IMon (VarToLoc, ReturnVal)
    -> IMon (VarToLoc, ReturnVal)
execCondAndActM e tFun fFun = do
    b <- evalExprM e
    case b of
        (Just cond) -> case cond of
            (VBool True ) -> tFun
            (VBool False) -> fFun
            _ ->
                throwM
                    $  "Illegal expression for while loop condition: " -- TODO: nazwa while jest malo uniwersAlna xD
                    ++ show e
        Nothing ->
            throwM $ "Invalid expression for while loop condition: " ++ show e


execStmtsM :: [Stmt] -> IMon (VarToLoc, ReturnVal)
execStmtsM []       = continueInCurrentEnv
execStmtsM (s : xs) = do
    (possiblyUpdatedEnv, ret) <- execStmtM s
    case ret of
        Nothing ->
            -- liftIO $ putStrLn "NOTHINGrETURNED~  "
            local (const possiblyUpdatedEnv) (execStmtsM xs) -- always run in 'new' env, which is only sometimes changed
        _ -> return (possiblyUpdatedEnv, ret) -- (?) function return (?)

runInterpreter (Program prog) = runStateT
    (runReaderT (execStmtsM prog) M.empty)
    initialState
    where initialState = IMState M.empty 0


-- itemsToVarVal :: Type -> [Item] -> IMon ([(Var, Val)])
-- itemsToVarVal t = return Prelude.map (toVarVal t)

-- toVarVal :: Type -> Item -> IMon () -> (Var, Val)
-- toVarVal Int  (DefaultInit (Ident var)     ) = (var, VInt 0)
-- toVarVal Bool (DefaultInit (Ident var)     ) = (var, VBool False)
-- toVarVal Str  (DefaultInit (Ident var)     ) = (var, VString "")
-- toVarVal Void (DefaultInit _) = error "VOID DEFAULT DECLARATION"
-- toVarVal _ (DefaultInit _) = error "DEFAULT FUNC DECL ILLEGAL"

-- toVarVal Int  (Init (Ident var) (ELitInt val)) = (var, VInt val)
-- toVarVal Bool (Init (Ident var) ELitTrue   ) = (var, VBool True)
-- toVarVal Bool (Init (Ident var) ELitFalse  ) = (var, VBool False)
-- Decl Bool [Init (Ident "j") (ELitInt 1)]
-- data Type = Int | Str | Bool | Void | Fun [Type] Type


-- valToString (VInt  x) = show x
-- valToString (VBool   b) = show b
-- valToString (VString s) = s
-- valToString _       = error "ERROR TODO"


-- valToString (VInt  x) = show x
-- valToString (VBool   b) = show b
-- valToString (VString s) = s
-- valToString _       = "ERROR TODO"

  -- do
  -- env <- ask
  -- return (env, e')

-- execStmtM (Decl e) = do
--   env <- ask
--   let e' = evalExprM e env
--   modify (Map.insert x e')

-- TODO: to jest jakoś chujowo -- TODO: jednak jest kul -- XD

