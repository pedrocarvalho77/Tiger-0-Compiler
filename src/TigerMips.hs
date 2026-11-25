-- Geracao de assembly MIPS (SPIM/MARS) a partir do codigo intermedio.
-- Estrategia simples: cada temporario vive numa posicao da frame da funcao;
-- cada instrucao carrega os operandos para $t0/$t1 e guarda o resultado.
module TigerMips where

import Interm
import Parser
import Data.List (elemIndex, nub, partition)
import Data.Maybe (fromJust)

-- Instrucao de salto condicional. "first" e o salto para a etiqueta falsa
-- (usado quando a etiqueta verdadeira vem logo a seguir), "second" o salto
-- para a etiqueta verdadeira.
opMips :: String -> RelOp -> String
opMips "first" op
  | op == Lt = "bge"
  | op == Lteq = "bgt"
  | op == Gt = "ble"
  | op == Gteq = "blt"
  | op == Diff = "beq"
  | otherwise = "bne"

opMips "second" op
  | op == Lt = "blt"
  | op == Lteq = "ble"
  | op == Gt = "bgt"
  | op == Gteq = "bge"
  | op == Diff = "bne"
  | otherwise = "beq"

opMips _ _ = error "opMips: \"first\" ou \"second\""

-- main primeiro: o simulador comeca a executar no inicio do .text.
compile :: Program -> String
compile prog = unlines (dataSection prog ++ ["", "\t.text", "\t.globl main"]
                        ++ concatMap transFunc (mains ++ others) ++ runtime)
  where (mains, others) = partition (\(Func n _ _) -> n == "main") (progFuncs prog)

dataSection :: Program -> [String]
dataSection prog = "\t.data" : [g ++ ":\t.word 0" | g <- progGlobals prog]
                   ++ [l ++ ":\t.asciiz \"" ++ concatMap esc s ++ "\"" | (l, s) <- progStrings prog]
  where esc '\n' = "\\n"
        esc '\t' = "\\t"
        esc '"'  = "\\\""
        esc '\\' = "\\\\"
        esc c    = [c]

-- Temporarios de uma funcao: um por posicao da frame.
frameTemps :: [Temp] -> [Instr] -> [Temp]
frameTemps params code = nub (params ++ concatMap temps code)
  where
    temps ins = case ins of
      MOVE a b -> [a, b]
      MOVEI a _ -> [a]
      OP _ a b c -> [a, b, c]
      OPI _ a b _ -> [a, b]
      COND a _ b _ _ -> [a, b]
      CALL d _ as -> d : as
      RETURN t -> [t]
      LOAD a b c -> [a, b, c]
      STORE a b c -> [a, b, c]
      GLOAD a _ -> [a]
      GSTORE _ a -> [a]
      STRADDR a _ -> [a]
      _ -> []

transFunc :: Func -> [String]
transFunc (Func name params code) =
  [name ++ ":"]
  ++ (if isMain then [] else ["\taddiu $sp, $sp, -" ++ show size, "\tsw $ra, " ++ show (size - 4) ++ "($sp)"])
  ++ (if isMain then ["\taddiu $sp, $sp, -" ++ show size] else [])
  ++ ["\tsw $a" ++ show i ++ ", " ++ slot p | (p, i) <- zip params [0 :: Int ..]]
  ++ concat (zipWith instr code (map Just (drop 1 code) ++ [Nothing]))
  where
    isMain = name == "main"
    ts = frameTemps params code
    size = 4 * length ts + 8
    slot t = show (4 * fromJust (elemIndex t ts)) ++ "($sp)"
    load r t = "\tlw " ++ r ++ ", " ++ slot t
    store r t = "\tsw " ++ r ++ ", " ++ slot t
    ret | isMain = ["\tli $v0, 10", "\tsyscall"]
        | otherwise = ["\tlw $ra, " ++ show (size - 4) ++ "($sp)",
                       "\taddiu $sp, $sp, " ++ show size, "\tjr $ra"]

    arith op = case op of
      Plus -> ["\taddu $t0, $t0, $t1"]
      Minus -> ["\tsubu $t0, $t0, $t1"]
      Times -> ["\tmul $t0, $t0, $t1"]
      Div -> ["\tdiv $t0, $t1", "\tmflo $t0"]
      Mod -> ["\tdiv $t0, $t1", "\tmfhi $t0"]

    instr ins next = case ins of
      MOVE a b -> [load "$t0" b, store "$t0" a]
      MOVEI a n -> ["\tli $t0, " ++ show n, store "$t0" a]
      OP op a b c -> [load "$t0" b, load "$t1" c] ++ arith op ++ [store "$t0" a]
      OPI op a b n -> [load "$t0" b, "\tli $t1, " ++ show n] ++ arith op ++ [store "$t0" a]
      LABEL l -> [l ++ ":"]
      JUMP l -> ["\tj " ++ l]
      COND a op b lt lf
        | next == Just (LABEL lt) -> [load "$t0" a, load "$t1" b,
                                      "\t" ++ opMips "first" op ++ " $t0, $t1, " ++ lf]
        | otherwise -> [load "$t0" a, load "$t1" b,
                        "\t" ++ opMips "second" op ++ " $t0, $t1, " ++ lt, "\tj " ++ lf]
      CALL d f args -> call d f args
      RETURN t -> load "$v0" t : ret
      RETVOID -> ret
      LOAD a b i -> [load "$t0" b, load "$t1" i, "\tsll $t1, $t1, 2", "\taddu $t0, $t0, $t1",
                     "\tlw $t0, 0($t0)", store "$t0" a]
      STORE b i s -> [load "$t0" b, load "$t1" i, "\tsll $t1, $t1, 2", "\taddu $t0, $t0, $t1",
                      load "$t1" s, "\tsw $t1, 0($t0)"]
      GLOAD a g -> ["\tlw $t0, " ++ g, store "$t0" a]
      GSTORE g t -> [load "$t0" t, "\tsw $t0, " ++ g]
      STRADDR a l -> ["\tla $t0, " ++ l, store "$t0" a]

    call d f args
      | length args > 4 = error ("mais de 4 argumentos na chamada a " ++ f)
      | f == rtPrinti = [load "$a0" (head args), "\tli $v0, 1", "\tsyscall"]
      | f == rtPrint  = [load "$a0" (head args), "\tli $v0, 4", "\tsyscall"]
      | f == rtScani  = ["\tli $v0, 5", "\tsyscall", store "$v0" d]
      | otherwise     = [load ("$a" ++ show i) a | (a, i) <- zip args [0 :: Int ..]]
                        ++ ["\tjal " ++ f, store "$v0" d]

-- _newarray(n, v): aloca n palavras e preenche-as com v; devolve o endereco.
runtime :: [String]
runtime =
  [ "_newarray:"
  , "\tmove $t0, $a0"          -- n
  , "\tmove $t1, $a1"          -- v
  , "\tsll $a0, $a0, 2"
  , "\tli $v0, 9"
  , "\tsyscall"                -- $v0 = endereco
  , "\tmove $t2, $v0"
  , "_newarray_loop:"
  , "\tblez $t0, _newarray_end"
  , "\tsw $t1, 0($t2)"
  , "\taddiu $t2, $t2, 4"
  , "\taddiu $t0, $t0, -1"
  , "\tj _newarray_loop"
  , "_newarray_end:"
  , "\tjr $ra"
  ]
