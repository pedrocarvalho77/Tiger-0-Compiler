module Main where
import Parser
import Lexer

main :: IO ()
main = do
    txt <- getContents
    print (parser $ alexScanTokens txt)