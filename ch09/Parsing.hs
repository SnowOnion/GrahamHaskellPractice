-- Functional parsing library from chapter 8 of Programming in Haskell,
-- Graham Hutton, Cambridge University Press, 2007.

-- Adapted by Jubobs, 2015
-- Available at https://github.com/Jubobs/Hutton-functional-parsers

module Parsing where


import Data.Char           ( isDigit, isLower,    isUpper,
                             isAlpha, isAlphaNum, isSpace )
import Control.Applicative ( Applicative(..), Alternative(..) )
import Control.Monad       ( liftM, ap, MonadPlus(..) )

infixr 5 +++

-- The monad of parsers
-- --------------------

newtype Parser a              =  P (String -> [(a,String)])

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure  = return
  (<*>) = ap

instance Monad Parser where
   return v                   =  P (\inp -> [(v,inp)])
   p >>= f                    =  P (\inp -> case parse p inp of
                                               []        -> []
                                               [(v,out)] -> parse (f v) out)

instance Alternative Parser where
  (<|>) = mplus
  empty = mzero

instance MonadPlus Parser where
   mzero                      =  P (\inp -> [])
   p `mplus` q                =  P (\inp -> case parse p inp of
                                               []        -> parse q inp
                                               [(v,out)] -> [(v,out)])

-- Basic parsers
-- -------------

failure                       :: Parser a
failure                       =  mzero

item                          :: Parser Char
item                          =  P (\inp -> case inp of
                                               []     -> []
                                               (x:xs) -> [(x,xs)])

parse                         :: Parser a -> String -> [(a,String)]
parse (P p) inp               =  p inp

-- Choice
-- ------

(+++)                         :: Parser a -> Parser a -> Parser a
p +++ q                       =  p `mplus` q

-- Derived primitives
-- ------------------

sat                           :: (Char -> Bool) -> Parser Char
sat p                         =  do x <- item
                                    if p x then return x else failure

digit                         :: Parser Char
digit                         =  sat isDigit

lower                         :: Parser Char
lower                         =  sat isLower

upper                         :: Parser Char
upper                         =  sat isUpper

letter                        :: Parser Char
letter                        =  sat isAlpha

alphanum                      :: Parser Char
alphanum                      =  sat isAlphaNum

char                          :: Char -> Parser Char
char x                        =  sat (== x)

string                        :: String -> Parser String
string []                     =  return []
string (x:xs)                 =  do char x
                                    string xs
                                    return (x:xs)

ident                         :: Parser String
ident                         =  do x  <- lower
                                    xs <- many alphanum
                                    return (x:xs)

nat                           :: Parser Int
nat                           =  do xs <- some digit
                                    return (read xs)

int                           :: Parser Int
int                           =  do char '-'
                                    n <- nat
                                    return (-n)
                                  +++ nat

space                         :: Parser ()
space                         =  do many (sat isSpace)
                                    return ()

com                           :: Parser ()
com                           = do string "--"
                                   many (sat (/= '\n'))
                                   return ()

-- Ignoring spacing
-- ----------------

token                         :: Parser a -> Parser a
token p                       =  do space
                                    v <- p
                                    space
                                    return v

identifier                    :: Parser String
identifier                    =  token ident

natural                       :: Parser Int
natural                       =  token nat

integer                       :: Parser Int
integer                       =  token int

symbol                        :: String -> Parser String
symbol xs                     =  token (string xs)

comment                       :: Parser ()
comment                       = token com
