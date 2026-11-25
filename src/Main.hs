module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Parser
import Lexer
import Interm
import TigerMips
import TypeCheck

-- Le um programa Tiger-0 do stdin.
--   (sem opcoes)  escreve assembly MIPS
--   --ast         escreve a arvore sintatica (sem verificar tipos)
--   --ir          escreve o codigo intermedio
main :: IO ()
main = do
    args <- getArgs
    txt <- getContents
    let ast = parser (alexScanTokens txt)
    case args of
      ["--ast"] -> print ast
      ["--ir"]  -> checked ast $ mapM_ print (progFuncs (transProg ast))
      []        -> checked ast $ putStr (compile (transProg ast))
      _         -> hPutStrLn stderr "uso: tiger [--ast | --ir] < programa.tig" >> exitFailure

-- Verifica os tipos antes de gerar codigo.
checked :: Prog -> IO () -> IO ()
checked ast gen = case typeCheck ast of
    Left err -> hPutStrLn stderr ("erro de tipos: " ++ err) >> exitFailure
    Right () -> gen
