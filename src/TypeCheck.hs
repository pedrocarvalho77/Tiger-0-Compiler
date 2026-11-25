-- Analise semantica: verificacao de tipos e de ambito. Corre antes da geracao
-- de codigo; devolve a primeira mensagem de erro encontrada.
module TypeCheck (typeCheck) where

import Parser
import Control.Monad (foldM, forM_, unless, when)
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as Map

data Ty = TInt | TString | TIntArray | TUnit
  deriving Eq

instance Show Ty where
  show TInt = "int"
  show TString = "string"
  show TIntArray = "intArray"
  show TUnit = "sem valor"

data Env = Env
  { vars   :: Map Ident (Ty, Bool)   -- tipo e se e so de leitura (variavel de for)
  , funs   :: Map Ident ([Ty], Ty)
  , inLoop :: Bool
  }

type Check a = Either String a

typeCheck :: Prog -> Either String ()
typeCheck (LetIn decls body) = do
  let sigs = [(funName f, signature f) | FunDecl f <- decls]
  unless (length (nub (map fst sigs)) == length sigs) $
    Left "funcao declarada mais do que uma vez"
  let env0 = Env Map.empty (Map.fromList sigs) False
  env <- foldM decl env0 decls
  _ <- typeSeq env body
  return ()
  where
    decl env (VarDecl (Decl x e)) = do
      t <- infer env e
      when (t == TUnit) $ Left ("a variavel " ++ x ++ " e inicializada com uma expressao sem valor")
      return env { vars = Map.insert x (t, False) (vars env) }
    decl env (FunDecl f) = checkFun env f >> return env

funName :: Fun -> Ident
funName (FunDef f _ _) = f
funName (FunDefType f _ _ _) = f

params :: Fun -> [TpFields]
params (FunDef _ ps _) = ps
params (FunDefType _ ps _ _) = ps

signature :: Fun -> ([Ty], Ty)
signature f = ([tyOf t | TypeField _ t <- params f], ret)
  where ret = case f of
                FunDef {} -> TUnit
                FunDefType _ _ t _ -> tyOf t

tyOf :: Type -> Ty
tyOf TypeInt = TInt
tyOf TypeString = TString
tyOf TypeIntArray = TIntArray

checkFun :: Env -> Fun -> Check ()
checkFun env f = do
  let names = [x | TypeField x _ <- params f]
  unless (length (nub names) == length names) $
    Left ("parametros repetidos na funcao " ++ funName f)
  let vs = Map.fromList [(x, (tyOf t, False)) | TypeField x t <- params f]
      env' = env { vars = Map.union vs (vars env), inLoop = False }
      body = case f of FunDef _ _ b -> b
                       FunDefType _ _ _ b -> b
  t <- infer env' body
  expect ("corpo da funcao " ++ funName f) (snd (signature f)) t

expect :: String -> Ty -> Ty -> Check ()
expect what want got =
  unless (want == got) $
    Left (what ++ ": esperado " ++ show want ++ ", obtido " ++ show got)

-- Tipo de uma expressao com um valor (nao pode ser "sem valor").
value :: String -> Env -> Exp -> Check Ty
value what env e = do
  t <- infer env e
  when (t == TUnit) $ Left (what ++ ": a expressao nao produz valor")
  return t

intExp :: String -> Env -> Exp -> Check ()
intExp what env e = infer env e >>= expect what TInt

typeSeq :: Env -> [Exp] -> Check Ty
typeSeq _ [] = return TUnit
typeSeq env es = mapM (infer env) es >>= return . last

infer :: Env -> Exp -> Check Ty
infer _ (Num _) = return TInt
infer _ (Str _) = return TString
infer env (Var x) = fmap fst (lookupVar env x)

infer env (Index a i) = do
  (t, _) <- lookupVar env a
  expect ("indexacao de " ++ a) TIntArray t
  intExp "indice do array" env i
  return TInt

infer env (ArrayNew n v) = do
  intExp "tamanho do array" env n
  intExp "valor inicial do array" env v
  return TIntArray

infer env (NegExp e) = intExp "operando de '-'" env e >> return TInt
infer env (Not e) = intExp "argumento de not" env e >> return TInt

infer env (OpBin op e1 e2) = do
  intExp ("operando esquerdo de " ++ show op) env e1
  intExp ("operando direito de " ++ show op) env e2
  return TInt

infer env (OpRel op e1 e2)
  | op == And || op == Or = do
      intExp ("operando esquerdo de " ++ show op) env e1
      intExp ("operando direito de " ++ show op) env e2
      return TInt
  | otherwise = do
      t1 <- value ("operando esquerdo de " ++ show op) env e1
      t2 <- value ("operando direito de " ++ show op) env e2
      expect ("operandos de " ++ show op) t1 t2
      -- O gerador de codigo so compara inteiros (e referencias de arrays com = e <>).
      when (t1 == TString) $ Left "comparacao de strings nao suportada"
      when (t1 == TIntArray && op /= Eq && op /= Diff) $ Left ("operador " ++ show op ++ " nao se aplica a arrays")
      return TInt

infer env (Assign x e) = do
  (t, readOnly) <- lookupVar env x
  when readOnly $ Left ("a variavel de ciclo " ++ x ++ " nao pode ser alterada")
  t' <- value ("atribuicao a " ++ x) env e
  expect ("atribuicao a " ++ x) t t'
  return TUnit

infer env (AssignIndex a i e) = do
  (t, _) <- lookupVar env a
  expect ("indexacao de " ++ a) TIntArray t
  intExp "indice do array" env i
  intExp ("atribuicao a " ++ a ++ "[...]") env e
  return TUnit

infer env (FunCall f args) = case Map.lookup f (funs env) of
  Nothing -> Left ("funcao nao declarada: " ++ f)
  Just (ps, ret) -> do
    when (length ps /= length args) $
      Left ("a funcao " ++ f ++ " espera " ++ show (length ps) ++ " argumento(s), recebeu " ++ show (length args))
    forM_ (zip3 [1 :: Int ..] ps args) $ \(n, p, a) -> do
      t <- value ("argumento " ++ show n ++ " de " ++ f) env a
      expect ("argumento " ++ show n ++ " de " ++ f) p t
    return ret

infer env (ExprSeq es) = typeSeq env es

infer env (IfThenElse c e1 e2) = do
  intExp "condicao do if" env c
  t1 <- infer env e1
  t2 <- infer env e2
  expect "ramos do if-then-else" t1 t2
  return t1

infer env (IfThen c e) = do
  intExp "condicao do if" env c
  t <- infer env e
  expect "corpo do if-then" TUnit t
  return TUnit

infer env (WhileDo c e) = do
  intExp "condicao do while" env c
  t <- infer env { inLoop = True } e
  expect "corpo do while" TUnit t
  return TUnit

infer env (ForToDo i e1 e2 body) = do
  intExp "limite inferior do for" env e1
  intExp "limite superior do for" env e2
  t <- infer env { vars = Map.insert i (TInt, True) (vars env), inLoop = True } body
  expect "corpo do for" TUnit t
  return TUnit

infer env Break = do
  unless (inLoop env) $ Left "break fora de um ciclo"
  return TUnit

infer env (LetInEnd decls body) = do
  env' <- foldM bind env decls
  typeSeq env' body
  where
    bind en (Decl x e) = do
      t <- value ("inicializacao de " ++ x) en e
      return en { vars = Map.insert x (t, False) (vars en) }

infer env (PrintInt e) = intExp "argumento de printi" env e >> return TUnit
infer env (Print e) = infer env e >>= expect "argumento de print" TString >> return TUnit
infer _ ScanInt = return TInt

lookupVar :: Env -> Ident -> Check (Ty, Bool)
lookupVar env x = maybe (Left ("variavel nao declarada: " ++ x)) Right (Map.lookup x (vars env))
