module Tokenizer(Token, specialChars, tokenize) where

data Token = LParen | RParen | ListChar | NewLine | Literal [Char] | Word [Char] 

instance Show Token where
	show LParen      = show "("
	show RParen      = show ")"
	show ListChar    = show "\'"
	show NewLine     = show '\n'
	show (Literal x) = show x
	show (Word    x) = show x

specialChars :: [Char]
specialChars =  "()\'\"\\\n "

tokenize :: [Char] -> [Token]
tokenize []     = []
tokenize ('(' :xs) = LParen   : tokenize xs
tokenize (')' :xs) = RParen   : tokenize xs
tokenize ('\'':xs) = ListChar : tokenize xs 
tokenize ('\n':xs) = NewLine  : tokenize xs
tokenize ('"' :xs) = (Literal quote) : tokenize remaining
	where 
		quoteAux :: [Char] -> [Char] -> ([Char], [Char])
		quoteAux build ('\"':xs)   = (build, xs) 
		quoteAux build ('\\':y:ys)              
			| y == '"'  = quoteAux (build ++ [y])     ys
			| otherwise = quoteAux (build ++ ['\\']) (y:ys)
		quoteAux build (x   :xs)   = quoteAux (build ++ [x]) xs
		(quote, remaining) = quoteAux [] xs
tokenize (' ' :xs) = tokenize xs
tokenize xs        = (Word word) : tokenize remaining
	where
		takeWord :: [Char] -> [Char] -> ([Char], [Char])
		takeWord build []             = (build, [])
		takeWord build (x:xs)  
			| elem x specialChars = (build, x:xs)
			| otherwise           = takeWord (build ++ [x]) xs 
		(word, remaining) = takeWord [] xs














