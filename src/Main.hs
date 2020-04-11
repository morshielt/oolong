import           System.Environment             ( getArgs )
import           System.Exit                    ( exitFailure )

import           Control.Monad                  ( when )

import           ParOolong
import           PrintOolong
import           AbsOolong

import           TypeCheck
import           Interpreter


import           ErrM


putStrV :: Int -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

showTree :: (Show a, Print a) => Int -> a -> IO ()
showTree v tree = do
    putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
    putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree


-- driver

check :: String -> IO ()
check s = case pProgram (myLexer s) of
    Bad err -> do
        putStrLn "SYNTAX ERROR"
        putStrLn err
        exitFailure
    Ok tree -> do
        showTree 2 tree
        case typecheck tree of
            Bad err -> do
                putStrLn "TYPE ERROR"
                putStrLn err
                exitFailure
            Ok _ -> interpret tree

main :: IO ()
main = do
    args <- getArgs
    case args of
        [file] -> readFile file >>= check
        []     -> getContents >>= check
        _      -> do
            putStrLn "Usage: ??? <SourceFile>"
            exitFailure
