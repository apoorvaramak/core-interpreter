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

--- The Parser
--- ----------

-- Pretty parser type
type Parser = ParsecT String () Identity

--- ### Lexicals

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
expr = eLet
   <|> eAp
   <|> eCase
   <|> eLam

decl :: Parser Decl
decl = do name <- id
          params <- many id
          _ <- symbol "="
          body <- expr
          return (name,params,body)

core :: Parser Core
core = do decls <- decl `sepBy` (symbol ";")
          return $ M.fromList [(n,v) | v@(n,_,_) <- decls]

parseCore :: String -> Either ParseError Core
parseCore text = parse core "Core" text

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

aExpr :: Parser Expr
aExpr = eVar
    <|> eInt
    <|> ePack
    <|> parens expr

eAp :: Parser Expr
eAp = do 
   expressions <- many1 aexpr
   return foldl1 EAp expressions

define :: Parser (Name, Expr)
define = do
   key <- id
   _ <- symbol "="
   value <- expr
   return (key, value)

eLet :: Parser Expr
eLet = do 
IsRec <- (symbol "letrec" >> return True)
   <|> (symbol "let" >> return False)
defs <- many1 define
_ <- symbol "in"
body <- expr
return $ ELet IsRec defs body

eAltCase :: Parser (Int, [Name], Expr)
eAltCase = do
   _ <- symbol "<"
   param <- int
   _ <- symbol ">"
   body <- many id
   _ <- symbol "->"
   altCaseExpr <- expr
   return (param, body, altCaseExpr)

eCase :: Parser Expr
eCase = do
   _ <- symbol "case"
   matchExpr <- expr
   _ <- symbol "of"
   altCases <- many1 eAltCase
   return $ ECase matchExpr altCases

eLam :: Parser Expr
eLam = do
   _ <- symbol "\\"
   params <- many1 id
   _ <- symbol "."
   body <- expr
   return $ eLam params body

