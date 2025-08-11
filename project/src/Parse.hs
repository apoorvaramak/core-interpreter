-- | The parser goes here

module Parse where

import Prelude hiding (id)

import Types

import qualified Data.HashMap.Lazy as M

import Data.Functor.Identity
import Control.Monad
import Data.Functor.Identity (Identity)
import Text.ParserCombinators.Parsec hiding (Parser)
import Text.Parsec.Prim (ParsecT)
import Data.List (partition)


type Parser = ParsecT String () Identity

symbol :: String -> Parser String
symbol s = do string s
              spaces
              return s

int :: Parser Int
int = do digits <- many1 digit <?> "an integer"
         spaces
         return (read digits :: Int)

id :: Parser Name
id = do first <- oneOf ['a' .. 'z']
        rest <- many (oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "'_")
        spaces
        return $ first:rest

expr :: Parser Expr
expr = orExpr

orExpr :: Parser Expr
orExpr = do
  left <- andExpr
  rest <- many (do _ <- symbol "|"
                   right <- andExpr
                   return right)
  return $ foldl (\l r -> EAp (EAp (EVar "|") l) r) left rest

andExpr :: Parser Expr
andExpr = do
  left <- eqExpr
  rest <- many (do _ <- symbol "&"
                   right <- eqExpr
                   return right)
  return $ foldl (\l r -> EAp (EAp (EVar "&") l) r) left rest

eqExpr :: Parser Expr
eqExpr = do
  left <- relExpr
  rest <- many (do op <- (symbol "==" <|> symbol "~=")
                   right <- relExpr
                   return (op, right))
  return $ foldl (\l (op, r) -> EAp (EAp (EVar op) l) r) left rest

relExpr :: Parser Expr
relExpr = do
  left <- addExpr
  rest <- many (do op <- (try (symbol "<=") <|> try (symbol ">=") <|> symbol "<" <|> symbol ">")
                   right <- addExpr
                   return (op, right))
  return $ foldl (\l (op, r) -> EAp (EAp (EVar op) l) r) left rest

addExpr :: Parser Expr
addExpr = do
  left <- mulExpr
  rest <- many (do op <- (try (symbol "+") <|> try (symbol "-"))
                   right <- mulExpr
                   return (op, right))
  return $ foldl (\l (op, r) -> EAp (EAp (EVar op) l) r) left rest

mulExpr :: Parser Expr
mulExpr = do
  left <- appExpr
  rest <- many (do op <- (symbol "*" <|> symbol "/")
                   right <- appExpr
                   return (op, right))
  return $ foldl (\l (op, r) -> EAp (EAp (EVar op) l) r) left rest

appExpr :: Parser Expr
appExpr = do
  func <- atom
  args <- many (try atom)
  return $ foldl EAp func args

atom :: Parser Expr
atom =  eInt
    <|> eVar
    <|> ePack
    <|> eLet
    <|> eCase
    <|> eLam
    <|> parenExpr

eInt :: Parser Expr
eInt = do i <- int
          return $ ENum i

eVar :: Parser Expr
eVar = EVar <$> id

ePack :: Parser Expr
ePack = do
        _ <- symbol "Pack"
        _ <- symbol "{"
        fst <- int
        _ <- symbol ","
        snd <- int
        _ <- symbol "}"
        return $ EPack fst snd

parens :: Parser a -> Parser a
parens p = do symbol "("
              pp <- p
              symbol ")"
              return pp

parenExpr :: Parser Expr
parenExpr = parens expr

--- ### Special Expressions

eLet :: Parser Expr
eLet = do 
   isRec <- (symbol "letrec" >> return True)
      <|> (symbol "let" >> return False)
   defs <- many1 define
   _ <- symbol "in"
   body <- expr
   return $ ELet isRec defs body

define :: Parser (Name, Expr)
define = do
   key <- id
   _ <- symbol "="
   value <- expr
   return (key, value)

eCase :: Parser Expr
eCase = do
   _ <- symbol "case"
   matchExpr <- expr
   _ <- symbol "of"
   altCases <- eAltCase `sepBy1` (symbol ";")
   return $ ECase matchExpr altCases

eAltCase :: Parser (Int, [Name], Expr)
eAltCase = do
   _ <- symbol "<"
   param <- int
   _ <- symbol ">"
   body <- many id
   _ <- symbol "->"
   altCaseExpr <- expr
   return (param, body, altCaseExpr)

eLam :: Parser Expr
eLam = do
   _ <- symbol "\\"
   params <- many1 id
   _ <- symbol "."
   body <- expr
   return $ ELam params body

--- ### Declarations

declaration :: Parser (Either [(Name, Int)] Decl)
declaration = try typeDeclaration <|> try regularDeclaration
  where
    typeDeclaration = do
      result <- typeDecl
      return $ Left result
    regularDeclaration = do
      result <- decl
      return $ Right result

typeDecl :: Parser [(Name, Int)]
typeDecl = do
  _ <- spaces
  typeName <- id
  _ <- symbol "::="
  constructors <- constructorDecl `sepBy1` (symbol "|")
  return constructors
  where
    constructorDecl = do
      name <- id
      params <- many id
      return (name, length params)

decl :: Parser Decl
decl = do 
  name <- id
  params <- many id
  _ <- symbol "="
  body <- expr
  return (name, params, body)

--- ### Core Parser

core :: Parser Core
core = do 
  spaces
  declarations <- sepBy declaration (spaces >> char ';' >> spaces)
  spaces
  let (typeDecls, funcDecls) = partitionDeclarations declarations
      constructorList = concat typeDecls
      constructorMap = M.fromList [(name, (tag, arity)) | ((name, arity), tag) <- zip constructorList [1..]]
      constructorDecls = [(name, [], EPack tag arity) | (name, (tag, arity)) <- M.toList constructorMap]
      allDecls = constructorDecls ++ funcDecls
  return $ M.fromList [(n, v) | v@(n, _, _) <- allDecls]
  where
    partitionDeclarations [] = ([], [])
    partitionDeclarations (Left typeDecl : rest) = 
      let (types, funcs) = partitionDeclarations rest
      in (typeDecl : types, funcs)
    partitionDeclarations (Right funcDecl : rest) = 
      let (types, funcs) = partitionDeclarations rest
      in (types, funcDecl : funcs)

parseCore :: String -> Either ParseError Core
parseCore text = parse core "Core" text
