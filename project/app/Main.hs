-- --- Given Executable Code
-- --- =====================

-- module Main where

-- --- Initial State
-- --- -------------

import System.IO (hFlush, stdout)
import Parse
import Interp
import Types

-- rep :: IO ()
-- rep = do
--   text <- getContents                                 -- Read
--   case parseCore text of                              -- Parse
--     Left err -> print err                             -- Diagnostics
--     Right program -> do
--       print program
--       print $ run program   -- Eval

-- repl :: Env -> IO ()
-- repl = undefined -- Implement this is you want a REPL instead of a batch processor


-- main = do putStrLn "Welcome to your Core interpreter!"
--           putStrLn ""
--           putStrLn "Enter your program here and hit ^D when done."
--           rep s

type Name = String

data CoreExpr = ENum Int | EPack Int Int | EVar Name | EAp CoreExpr CoreExpr deriving Show

type CoreScDefn = (Name, [Name], CoreExpr)

type ConstructorEnv = M.Map Name (Int, Int)

maybeConstructors :: ConstructorEnv
maybeConstructors = M.fromList [("Just", (1,1)), ("None", (2,0))]

mainSc :: CoreScDefn
mainSc = ("main", [], ENum 4)

eval :: CoreExpr -> Int
eval (ENum n) = n
eval _ = error "Unsupported expression for this test"

main :: IO ()
main = do
  putStrLn "Constructors:"
  mapM_ print (M.toList maybeConstructors)
  let (_, _, body) = mainSc
  let result = eval body
  putStrLn $ "Evaluated main: " ++ show result

