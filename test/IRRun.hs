-- Interpretador do codigo intermedio, so para testes: nao precisa de simulador MIPS.
-- Uso: IRRun ficheiro.tig   (scani le inteiros do stdin)
module Main where

import Data.IORef
import qualified Data.Map as Map
import System.Environment (getArgs)
import Lexer
import Parser
import Interm

type Mem = Map.Map Int Int

main :: IO ()
main = do
  [file] <- getArgs
  src <- readFile file
  let prog = transProg (parser (alexScanTokens src))
      funcs = Map.fromList [(l, f) | f@(Func l _ _) <- progFuncs prog]
      strAddrs = Map.fromList (zip (map fst (progStrings prog)) [1000000, 1000001 ..])
      strTexts = Map.fromList (zip [1000000 ..] (map snd (progStrings prog)))
  globals <- newIORef (Map.empty :: Map.Map Label Int)
  heap <- newIORef (Map.empty :: Mem)
  next <- newIORef (1 :: Int)
  input <- fmap (map read . words) getContents >>= newIORef

  let call :: Label -> [Int] -> IO Int
      call "_printi" [n] = putStr (show n) >> return 0
      call "_print" [a] = putStr (strTexts Map.! a) >> return 0
      call "_scani" [] = do (x:xs) <- readIORef input; writeIORef input xs; return x
      call "_newarray" [n, v] = do
        base <- readIORef next
        writeIORef next (base + n)
        modifyIORef heap (\h -> foldr (\i -> Map.insert (base + i) v) h [0 .. n - 1])
        return base
      call l args = run (funcs Map.! l) args

      run (Func _ params code) args = exec 0 (Map.fromList (zip params args))
        where
          labelIdx = Map.fromList [(l, i) | (LABEL l, i) <- zip code [0 ..]]
          jump l = exec (labelIdx Map.! l)
          exec pc env
            | pc >= length code = return 0
            | otherwise = step (code !! pc) env pc
          val env t = env Map.! t
          step ins env pc = case ins of
            MOVE a b -> exec (pc + 1) (Map.insert a (val env b) env)
            MOVEI a n -> exec (pc + 1) (Map.insert a n env)
            OP op a b c -> exec (pc + 1) (Map.insert a (arith op (val env b) (val env c)) env)
            OPI op a b n -> exec (pc + 1) (Map.insert a (arith op (val env b) n) env)
            LABEL _ -> exec (pc + 1) env
            JUMP l -> jump l env
            COND a op b lt lf -> jump (if rel op (val env a) (val env b) then lt else lf) env
            CALL d f as -> do r <- call f (map (val env) as)
                              exec (pc + 1) (Map.insert d r env)
            RETURN t -> return (val env t)
            RETVOID -> return 0
            LOAD d b i -> do h <- readIORef heap
                             exec (pc + 1) (Map.insert d (h Map.! (val env b + val env i)) env)
            STORE b i s -> do modifyIORef heap (Map.insert (val env b + val env i) (val env s))
                              exec (pc + 1) env
            GLOAD d g -> do gs <- readIORef globals
                            exec (pc + 1) (Map.insert d (Map.findWithDefault 0 g gs) env)
            GSTORE g t -> do modifyIORef globals (Map.insert g (val env t))
                             exec (pc + 1) env
            STRADDR d l -> exec (pc + 1) (Map.insert d (strAddrs Map.! l) env)

  _ <- call "main" []
  putStrLn ""

arith :: BinOp -> Int -> Int -> Int
arith Plus = (+)
arith Minus = (-)
arith Times = (*)
arith Div = quot
arith Mod = rem

rel :: RelOp -> Int -> Int -> Bool
rel Eq = (==)
rel Diff = (/=)
rel Lt = (<)
rel Lteq = (<=)
rel Gt = (>)
rel Gteq = (>=)
rel op = error ("operador inesperado: " ++ show op)
