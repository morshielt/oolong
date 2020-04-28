import           System.Environment             ( getArgs )
import           System.Exit                    ( exitFailure )
import           System.IO                      ( stderr
                                                , hPutStrLn
                                                )
import           Control.Monad.Except

import           ParOolong
import           AbsOolong

import           TypeCheck
import           Interpreter

import           ErrM

import           Control.Monad                  ( when )
import           PrintOolong

check :: String -> IO ()
check s = case pProgram (myLexer s) of
    Bad err -> do
        hPutStrLn stderr $ "[Syntax error] " ++ err
        exitFailure
    Ok tree -> do
        tcRes <- runExceptT $ runTypeChecker tree
        case tcRes of
            Left e -> do
                hPutStrLn stderr $ "[Typecheck exception] " ++ e
                exitFailure
            Right _ -> do
                res <- runExceptT $ runInterpreter tree
                case res of
                    Left e -> do
                        hPutStrLn stderr $ "[Runtime exception] " ++ e
                        exitFailure
                    Right _ -> return ()

main :: IO ()
main = do
    args <- getArgs
    case args of
        [file] -> readFile file >>= check
        []     -> getContents >>= check
        _      -> do
            putStrLn "Usage: ??? <SourceFile>"
            exitFailure
