module Main (main) where

import Tokenizer
import System.Environment

compile = show . Tokenizer.tokenize

main = do
	args <- getArgs
	text <- readFile $ foldr (++) "" args
	putStrLn $ compile text


