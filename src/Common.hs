module Common where

import           Control.Monad                  ( when )
import           PrintOolong

-- TODO: na koniec: używać tego do wyświetlania niepoprawnych rzeczy!
showTree :: (Show a, Print a) => a -> IO ()
showTree tree = do
    putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
    putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree
  where
    putStrV :: Int -> String -> IO ()
    putStrV v s = when (v > 1) $ putStrLn s
    v = 2

