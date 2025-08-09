-- | Interp

module Interp where

import qualified Data.HashMap.Lazy as M

import Types

intOps :: M.HashMap String (Int -> Int -> Int)
intOps = M.fromList [ ("+", (+))
                    , ("-", (-))
                    , ("*", (*))
                    , ("/", (div))
                    ]

boolOps :: M.HashMap String (Bool -> Bool -> Bool)
boolOps = M.fromList [ ("&", (&&))
                     , ("|", (||))
                     ]

compOps :: M.HashMap String (Int -> Int -> Bool)
compOps = M.fromList [ ("<", (<))
                     , (">", (>))
                     , ("<=", (<=))
                     , (">=", (>=))
                     , ("!=", (/=))
                     , ("==", (==))
                     ]

liftIntOp :: (Int -> Int -> Int) -> Val -> Val -> Val
liftIntOp op (IntVal x) (IntVal y) = IntVal $ op x y
liftIntOp _ _ _ = error "Cannot lift"

liftBoolOp :: (Bool -> Bool -> Bool) -> Val -> Val -> Val
liftBoolOp op (BoolVal x) (BoolVal y) = BoolVal $ op x y
liftBoolOp _ _ _ = error "Cannot lift"

liftCompOp :: (Int -> Int -> Bool) -> Val -> Val -> Val
liftCompOp op (IntVal x) (IntVal y) = BoolVal $ op x y
liftCompOp _ _ _ = error "Cannot lift"

eval :: Expr -> Env -> Val -- You will almost certainly need to change this to use your own Value type.
eval (ENum i) _ = IntVal i

eval (EVar name) env = case name of
  "True"  -> BoolVal True
  "False" -> BoolVal False
  _       -> case M.lookup name env of
               Just val -> val
               Nothing  -> error $ "Variable not found: " ++ name

eval (ELam params body) env = CloVal params body env

eval (EAp (EAp (EVar op) e1) e2) env
  | Just f <- M.lookup op intOps =
      let v1 = eval e1 env
          v2 = eval e2 env
      in liftIntOp f v1 v2

  | Just f <- M.lookup op compOps =
      let v1 = eval e1 env
          v2 = eval e2 env
      in liftCompOp f v1 v2

  | Just f <- M.lookup op boolOps =
      let v1 = eval e1 env
          v2 = eval e2 env
      in liftBoolOp f v1 v2

eval (EAp e1 e2) env =
  let v1 = eval e1 env
  in case v1 of
    CloVal (x:xs) body clenv -> 
      let args = eval e2 env
          clenv1 = M.insert x args clenv
          e3 = if (null xs) then body else ELam xs body
      in eval e3 clenv1
    _ -> error "applied to non-closure"

eval (ELet isRec pairs body) env =
  case isRec of
    False -> 
      let v1 = map (\(x, y) -> (x, eval y env)) pairs
          e1 = M.union (M.fromList v1) env
      in eval body e1
    _ -> 
      let e1 = M.union (M.fromList v1) env
          v1 = map (\(x, y) -> (x, eval y env)) pairs
      in eval body e1

-- eval (ECase e1 alts) env =
--   let v1 = eval e1 env
--   in case v1 of
--     ExnVal 
eval (ECase expr alts) env =
  case eval expr env of
    ExnVal tag args ->
      case lookupAlt tag alts of
        Just (params, altExpr) ->
          if length params == length args
            then
              let newEnv = M.union (M.fromList (zip params args)) env
              in eval altExpr newEnv
            else
              error $ "Constructor arity mismatch in case alternative for tag " ++ show tag
        Nothing ->
          error $ "No matching case alternative for tag " ++ show tag
    _ -> error "ECase applied to a non-constructor value"

eval (EPack x _) _ = ExnVal x []

lookupAlt :: Int -> [(Int, [Name], Expr)] -> Maybe ([Name], Expr)
lookupAlt _ [] = Nothing
lookupAlt tag ((t, params, expr):alts)
  | tag == t  = Just (params, expr)
  | otherwise = lookupAlt tag alts

-- Use this function as your top-level entry point so you don't break `app/Main.hs`

run :: Core -> String
run prog =
  case M.lookup "main" prog of
    Nothing -> error "Supercombinator main not defined."
    Just (_,[],mainBody) ->
      let result = eval mainBody (M.empty)
       in show result








