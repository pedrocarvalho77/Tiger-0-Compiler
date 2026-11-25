-- Traducao da AST para codigo intermedio (instrucoes de tres enderecos).
module Interm where

import Parser
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State

type Temp = String
type Label = String

data Instr = MOVE Temp Temp             -- temp1 := temp2
           | MOVEI Temp Int             -- temp1 := num
           | OP BinOp Temp Temp Temp    -- temp1 := temp2 op temp3
           | OPI BinOp Temp Temp Int    -- temp1 := temp2 op num
           | LABEL Label
           | JUMP Label
           | COND Temp RelOp Temp Label Label
           | CALL Temp Label [Temp]     -- temp := f(args)
           | RETURN Temp
           | RETVOID
           | LOAD Temp Temp Temp        -- temp1 := temp2[temp3]
           | STORE Temp Temp Temp       -- temp1[temp2] := temp3
           | GLOAD Temp Label           -- temp := variavel global
           | GSTORE Label Temp          -- variavel global := temp
           | STRADDR Temp Label         -- temp := endereco da string
           deriving (Eq, Show)

-- Funcao: etiqueta, parametros e corpo. O programa principal chama-se "main".
data Func = Func Label [Temp] [Instr]
  deriving Show

data Program = Program
  { progGlobals :: [Label]
  , progStrings :: [(Label, String)]
  , progFuncs   :: [Func]
  } deriving Show

-- Funcoes da biblioteca, implementadas no gerador de codigo.
rtPrint, rtPrinti, rtScani, rtNewArray :: Label
rtPrint    = "_print"
rtPrinti   = "_printi"
rtScani    = "_scani"
rtNewArray = "_newarray"   -- _newarray(n, v): novo array com n elementos a v

data Loc = Local Temp | Global Label

data Env = Env
  { vars  :: Map Ident Loc
  , funs  :: Map Ident Label
  , brk   :: Maybe Label     -- fim do ciclo mais interno (para o break)
  }

data Gen = Gen { temps :: Int, labels :: Int, strs :: [(Label, String)] }

type M a = State Gen a

newTemp :: M Temp
newTemp = do g <- get
             put g { temps = temps g + 1 }
             return ("t" ++ show (temps g))

newLabel :: M Label
newLabel = do g <- get
              put g { labels = labels g + 1 }
              return ("L" ++ show (labels g))

newString :: String -> M Label
newString s = do g <- get
                 let l = "str" ++ show (length (strs g))
                 put g { strs = strs g ++ [(l, s)] }
                 return l

-- O destino do resultado: Nothing quando o valor nao e usado.
destOrNew :: Maybe Temp -> M Temp
destOrNew = maybe newTemp return

-- Valor de uma expressao num temporario (sem copia se for variavel local).
asTemp :: Env -> Exp -> M (Temp, [Instr])
asTemp env (Var x)
  | Just (Local t) <- Map.lookup x (vars env) = return (t, [])
asTemp env e = do t <- newTemp
                  code <- transExp env (Just t) e
                  return (t, code)

lookupVar :: Env -> Ident -> Loc
lookupVar env x = case Map.lookup x (vars env) of
  Just loc -> loc
  Nothing  -> error ("variavel nao declarada: " ++ x)

transExp :: Env -> Maybe Temp -> Exp -> M [Instr]
transExp _ Nothing (Num _) = return []
transExp _ (Just d) (Num n) = return [MOVEI d n]

transExp _ Nothing (Str _) = return []
transExp _ (Just d) (Str s) = do l <- newString s
                                 return [STRADDR d l]

transExp _ Nothing (Var _) = return []
transExp env (Just d) (Var x) = case lookupVar env x of
  Local t  -> return [MOVE d t]
  Global l -> return [GLOAD d l]

transExp env dest (Index a i)
  = do d <- destOrNew dest
       (ta, c1) <- asTemp env (Var a)
       (ti, c2) <- asTemp env i
       return (c1 ++ c2 ++ [LOAD d ta ti])

transExp env dest (ArrayNew n v)
  = do d <- destOrNew dest
       (tn, c1) <- asTemp env n
       (tv, c2) <- asTemp env v
       return (c1 ++ c2 ++ [CALL d rtNewArray [tn, tv]])

transExp env dest (NegExp e)
  = do d <- destOrNew dest
       (t, c) <- asTemp env e
       z <- newTemp
       return (c ++ [MOVEI z 0, OP Minus d z t])

transExp env dest (OpBin op e1 e2)
  = do d <- destOrNew dest
       case (e1, e2) of
         (_, Num n) -> do (t1, c1) <- asTemp env e1
                          return (c1 ++ [OPI op d t1 n])
         (Num n, _) | op == Plus || op == Times
                    -> do (t2, c2) <- asTemp env e2
                          return (c2 ++ [OPI op d t2 n])
         _ -> do (t1, c1) <- asTemp env e1
                 (t2, c2) <- asTemp env e2
                 return (c1 ++ c2 ++ [OP op d t1 t2])

transExp env dest e@(OpRel _ _ _) = boolValue env dest e
transExp env dest e@(Not _) = boolValue env dest e

transExp env _ (Assign x e)
  = case lookupVar env x of
      Local t  -> transExp env (Just t) e
      Global l -> do (t, c) <- asTemp env e
                     return (c ++ [GSTORE l t])

transExp env _ (AssignIndex a i e)
  = do (ta, c1) <- asTemp env (Var a)
       (ti, c2) <- asTemp env i
       (te, c3) <- asTemp env e
       return (c1 ++ c2 ++ c3 ++ [STORE ta ti te])

transExp env dest (FunCall f args)
  = case Map.lookup f (funs env) of
      Nothing -> error ("funcao nao declarada: " ++ f)
      Just l -> do d <- destOrNew dest
                   rs <- mapM (asTemp env) args
                   return (concatMap snd rs ++ [CALL d l (map fst rs)])

transExp env dest (ExprSeq es) = transSeq env dest es

transExp env dest (IfThenElse cond e1 e2)
  = do ltrue <- newLabel
       lfalse <- newLabel
       lend <- newLabel
       code0 <- transCond env cond ltrue lfalse
       code1 <- transExp env dest e1
       code2 <- transExp env dest e2
       return (code0 ++ [LABEL ltrue] ++ code1 ++
               [JUMP lend, LABEL lfalse] ++ code2 ++ [LABEL lend])

transExp env _ (IfThen cond e1)
  = do ltrue <- newLabel
       lfalse <- newLabel
       code0 <- transCond env cond ltrue lfalse
       code1 <- transExp env Nothing e1
       return (code0 ++ [LABEL ltrue] ++ code1 ++ [LABEL lfalse])

transExp env _ (WhileDo cond body)
  = do lstart <- newLabel
       lbody <- newLabel
       lend <- newLabel
       code0 <- transCond env cond lbody lend
       code1 <- transExp env { brk = Just lend } Nothing body
       return ([LABEL lstart] ++ code0 ++ [LABEL lbody] ++ code1 ++
               [JUMP lstart, LABEL lend])

transExp env _ (ForToDo i e1 e2 body)
  = do ti <- newTemp
       tlimit <- newTemp
       lcond <- newLabel
       lbody <- newLabel
       lend <- newLabel
       code1 <- transExp env (Just ti) e1
       code2 <- transExp env (Just tlimit) e2
       let env' = env { vars = Map.insert i (Local ti) (vars env), brk = Just lend }
       code3 <- transExp env' Nothing body
       return (code1 ++ code2 ++ [LABEL lcond, COND ti Lteq tlimit lbody lend,
                                  LABEL lbody] ++ code3 ++
               [OPI Plus ti ti 1, JUMP lcond, LABEL lend])

transExp env _ Break
  = case brk env of
      Just l  -> return [JUMP l]
      Nothing -> error "break fora de um ciclo"

transExp env dest (LetInEnd decls body) = go env decls
  where
    go env' [] = transSeq env' dest body
    go env' (Decl x e : rest)
      = do t <- newTemp
           code <- transExp env' (Just t) e
           rest' <- go env' { vars = Map.insert x (Local t) (vars env') } rest
           return (code ++ rest')

transExp env _ (PrintInt e)
  = do (t, c) <- asTemp env e
       d <- newTemp
       return (c ++ [CALL d rtPrinti [t]])

transExp env _ (Print e)
  = do (t, c) <- asTemp env e
       d <- newTemp
       return (c ++ [CALL d rtPrint [t]])

transExp _ dest ScanInt
  = do d <- destOrNew dest
       return [CALL d rtScani []]

-- Valor 0/1 de uma condicao.
boolValue :: Env -> Maybe Temp -> Exp -> M [Instr]
boolValue env dest e
  = do d <- destOrNew dest
       ltrue <- newLabel
       lfalse <- newLabel
       code <- transCond env e ltrue lfalse
       return ([MOVEI d 0] ++ code ++ [LABEL ltrue, MOVEI d 1, LABEL lfalse])

-- Todas as expressoes menos a ultima descartam o valor.
transSeq :: Env -> Maybe Temp -> [Exp] -> M [Instr]
transSeq _ _ [] = return []
transSeq env dest [e] = transExp env dest e
transSeq env dest (e:es) = do c1 <- transExp env Nothing e
                              c2 <- transSeq env dest es
                              return (c1 ++ c2)

-- Salta para ltrue ou lfalse conforme o valor da condicao (& e | sao preguicosos).
transCond :: Env -> Exp -> Label -> Label -> M [Instr]
transCond env (OpRel And e1 e2) ltrue lfalse
  = do lnext <- newLabel
       code1 <- transCond env e1 lnext lfalse
       code2 <- transCond env e2 ltrue lfalse
       return (code1 ++ [LABEL lnext] ++ code2)

transCond env (OpRel Or e1 e2) ltrue lfalse
  = do lnext <- newLabel
       code1 <- transCond env e1 ltrue lnext
       code2 <- transCond env e2 ltrue lfalse
       return (code1 ++ [LABEL lnext] ++ code2)

transCond env (Not e) ltrue lfalse = transCond env e lfalse ltrue

transCond env (OpRel op e1 e2) ltrue lfalse
  = do (t1, c1) <- asTemp env e1
       (t2, c2) <- asTemp env e2
       return (c1 ++ c2 ++ [COND t1 op t2 ltrue lfalse])

transCond env e ltrue lfalse
  = do (t, c) <- asTemp env e
       z <- newTemp
       return (c ++ [MOVEI z 0, COND t Diff z ltrue lfalse])

-- As variaveis do let exterior sao globais (as funcoes veem-nas); o corpo do
-- programa torna-se a funcao "main".
transProg :: Prog -> Program
transProg (LetIn decls body) = evalState go (Gen 0 0 [])
  where
    globalVars = [x | VarDecl (Decl x _) <- decls]
    funDecls   = [f | FunDecl f <- decls]
    funName (FunDef f _ _) = f
    funName (FunDefType f _ _ _) = f
    env0 = Env { vars = Map.fromList [(x, Global ("g_" ++ x)) | x <- globalVars]
               , funs = Map.fromList [(funName f, "fn_" ++ funName f) | f <- funDecls]
               , brk  = Nothing }

    go = do fs <- mapM (transFun env0) funDecls
            initCode <- fmap concat (mapM (transGlobal env0) [v | VarDecl v <- decls])
            bodyCode <- transSeq env0 Nothing body
            g <- get
            return Program { progGlobals = ["g_" ++ x | x <- globalVars]
                           , progStrings = strs g
                           , progFuncs   = fs ++ [Func "main" [] (initCode ++ bodyCode ++ [RETVOID])] }

    transGlobal env (Decl x e)
      = do (t, c) <- asTemp env e
           return (c ++ [GSTORE ("g_" ++ x) t])

transFun :: Env -> Fun -> M Func
transFun env (FunDef f params body) = do
  (ps, env') <- bindParams env params
  code <- transExp env' Nothing body
  return (Func ("fn_" ++ f) ps (code ++ [RETVOID]))
transFun env (FunDefType f params _ body) = do
  (ps, env') <- bindParams env params
  t <- newTemp
  code <- transExp env' (Just t) body
  return (Func ("fn_" ++ f) ps (code ++ [RETURN t]))

bindParams :: Env -> [TpFields] -> M ([Temp], Env)
bindParams env params = do
  ts <- mapM (const newTemp) params
  let names = [x | TypeField x _ <- params]
  return (ts, env { vars = Map.union (Map.fromList (zip names (map Local ts))) (vars env) })
