module Parser ( ) where

import Control.Monad
    

data Parser a = Parser (String -> [(a, String)])

-- (|||) :: forall t Parser t -> Parser t -> String -> [(t, String)]
-- (Parser a) ||| (Parser b) = \s -> a s ++ b s

-- pseq :: Parser a -> (a -> Parser b) -> Parser b
-- pseq (Parser a) k = \s -> let fres = a s
--                               unp (Parser p) = p
--                               kres =

parse (Parser p) = p
                                  
item :: Parser Char
item = Parser(\s->case s of
                 ""     -> []
                 (c:cs) -> [(c,cs)])

-- ma -> (a -> mb) -> mb
--p >>= f = Parser(\s -> concat [parse (f pa) str | (pa,str)<- parse p s])
       
ps = do c<-item
        item
        d<-item
        return (c,d)

-- ps = item >>= \c->item >> item >>= \d->return (c,d)       

       
instance Monad Parser where
  return a = Parser(\s -> [(a,s)])
  p >>= f  = Parser(\s -> concat [parse (f a) cs' | (a,cs')<-parse p s])



instance MonadPlus Parser where
    mzero = Parser (\cs->[])
    p `mplus` q = Parser (\cs -> parse p cs ++ parse q cs) 
             
sat p = do c <- item
           if p c then return c else mzero

char c = sat (c==)

string ""     = return ""
string (c:cs) = do{char c; string cs; return (c:cs)}