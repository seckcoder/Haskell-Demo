module MonadicParsing.Main where

import Control.Applicative hiding (many)
import Control.Monad
import Data.Char
import Data.List (intercalate)
import Debug.Trace

-- Implement a parser for arithmetic expressions

newtype Parser a = Parser { parse :: (String -> [(a, String)]) }


instance Functor Parser where
    fmap f p = pure f <*> p

instance Applicative Parser where
    pure a = Parser (\s -> [(a,s)])
    (<*>) = ap

instance Monad Parser where
    return = pure
    p >>= f = Parser (\ s -> concat [parse (f a) s' | (a,s') <- parse p s] )

instance Alternative Parser where
    empty = Parser (\_ -> [])
    (<|>) p1 p2 = Parser (\s -> parse p1 s ++ parse p2 s)

instance MonadPlus Parser where
    mzero = empty
    mplus = (<|>)

-- parse and select the first one
(+++) :: Parser a -> Parser a -> Parser a
(+++) p1 p2 = Parser (\s ->
    case parse (p1 `mplus` p2) s of
        [] -> []
        (x:xs) -> [x])
 
item :: Parser Char
item = Parser (\ cs ->
    case cs of
        "" -> []
        (c:cs) -> [(c,cs)])

sat :: (Char -> Bool) -> Parser Char
sat p = do
    c <- item
    if p c then return c else mzero

char :: Char -> Parser Char
char c = sat (c ==)

count :: Int -> Parser a -> Parser [a]
count 0 p = return []
count k p = do
    v <- p
    vs <- count (k-1) p
    return (v:vs)
 
string :: String -> Parser String
string "" = return ""
string (c:cs) = do
    char c
    string cs
    return (c:cs)

-- parse 0 or more (as many as possible)
many :: Parser a -> Parser [a]
many p = many1 p +++ return []

-- parse 1 or more (as many as possible)
many1 :: Parser a -> Parser [a]
many1 p = do
    a <- p
    as <- many p
    return (a:as)

sepby :: Parser a -> Parser b -> Parser [a]
p `sepby` sep = (p `sepby1` sep) +++ return []

sepby1 :: Parser a -> Parser b -> Parser [a]
p `sepby1` sep = do
    a <- p
    as <- many (do {sep; p})
    return (a:as)

-- chainl apply a left-associative operator with the parsing result for
-- zero or more times (as many as possible)
chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) +++ return a

-- chainl1 apply a left-associative operator with the parsing result
-- for one or more times
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do { a <- p; rest a }
                    where
                        rest a = (do f <- op
                                     b <- p
                                     rest (f a b))
                                     +++ return a

space :: Parser String
space = many (sat isSpace)

token :: Parser a -> Parser a
token p = do { a <- p; space ; return a }

symb :: String -> Parser String
symb cs = token (string cs)

apply :: Parser a -> String -> [(a,String)]
apply p = parse ( do {space; p} )

--------- examples  -------------

-- Arithmetic Expression

{-

The ambiguous grammar is:
expr ::= expr op expr
     ::= digit | (expr)
      
The following is the unambiguous grammar(but left-recursive)

expr ::= expr addop term | term
term ::= term mulop factor | factor
factor ::= digit | (expr)
digit ::= 0 | 1 | ... | 9
addop ::= + | -
mulop ::= * | /

-}
expr :: Parser Int
expr = term `chainl1` addop
term = factor `chainl1` mulop
factor = digit +++ do { symb "("; n <- expr; symb ")"; return n}
digit = do { x <- token (sat isDigit); return (ord x - ord '0')}

addop = do {symb "+"; return (+)} +++ do {symb "-"; return (-)}
mulop = do {symb "*"; return (*)} +++ do {symb "/"; return (div)}



-- A full-featured CSV Parser

type Record = [String]

data CSVFile = CSVFile [String] [Record]

instance Show CSVFile where
    show (CSVFile hs rs) = (intercalate "\t" hs) ++ "\n" ++ (intercalate "\n" [intercalate "\t" r | r <- rs])

csv :: Parser CSVFile
csv = do
    hs <- header
    crlf
    rs <- record `sepby` crlf
    many crlf
    return $ CSVFile hs rs

header :: Parser [String]
header = name `sepby` comma
record :: Parser [String]
record = field `sepby` comma
name :: Parser String
name = field
field :: Parser String
field = escaped `mplus` non_escaped
escaped :: Parser String
escaped = do
    dquote
    v <- many (textdata +++ comma +++ cr +++ lf +++ dquote2)
    dquote
    return v
crlf :: Parser Char
crlf = do {cr; lf}
cr :: Parser Char
cr = char (chr 0x0d) -- '\r'
lf :: Parser Char
lf = char (chr 0x0a) -- '\n'
dquote :: Parser Char
dquote = char (chr 0x22)
dquote2 :: Parser Char
dquote2 = do
    count 2 dquote
    return '"'

textdata :: Parser Char
textdata = sat (\c -> let v = ord c
                          in 0x20 <= v && v <= 0x21 ||
                              0x23 <= v && v <= 0x2b ||
                                  0x2d <= v && v <= 0x7E)

non_escaped :: Parser String
non_escaped = many textdata

comma :: Parser Char
comma = char (chr 0x2c)

csv_raw = "fn,ln,email,location,num\r\n\
           \John,Smith,john.smith@gmail.com,Los Angeles,1\r\n\
           \\"Alexandra \"\"Alex\"\"\",Menendez,alex.menendez@gmail.com,Miami,1\r\n\
           \Jane,Roberts,janer@msn.com,\"San Francisco, CA\",0\r\n\
           \\"\"\"Alexandra Alex\"\"\""


-- Multi-Level List Parsing

lst_raws = [
    "324",
    "[123,456,[788,799,833],[[]],10,[]]"]

-- Data Representation of multi-level list
data MList = Number Int
           | MList [MList]
            deriving (Show)

{-
mlist ::= Int
       | [ mlist (, mlist) ]
-}
mlist :: Parser MList
mlist = number +++ mlist1
number = do
    ds <- many1 digit
    return $ Number $ foldl (\ v a -> 10*v+a) 0 ds

mlist1 = do
    char '['
    mlst <- mlist `sepby` comma
    char ']'
    return $ MList mlst

main = do
    print $ apply expr " 1 - 2 * 3 + 4 * (5+7) "
    print $ parse csv csv_raw
    print $ map (parse mlist) lst_raws