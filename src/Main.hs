module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Parser
import Lexer
import Interm
import TigerMips

-- Le um programa Tiger-0 do stdin.
--   (sem opcoes)  escreve assembly MIPS
--   --ast         escreve a arvore sintatica
--   --ir          escreve o codigo intermedio
main :: IO ()
main = do
    args <- getArgs
    txt <- getContents
    let ast = parser (alexScanTokens txt)
    case args of
      []        -> putStr (compile (transProg ast))
      ["--ast"] -> print ast
      ["--ir"]  -> mapM_ print (progFuncs (transProg ast))
      _         -> hPutStrLn stderr "uso: tiger [--ast | --ir] < programa.tig" >> exitFailure
