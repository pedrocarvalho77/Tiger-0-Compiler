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
           | CALL Temp Label [Temp]
           | BREAK
           | RETURN Temp
           deriving (Eq, Show)

type Table = Map Ident String
type Count = (Int,Int)

newTemp :: State Count Temp
newTemp = do (temps,labels) <- get
             put (temps+1,labels)
             return ("t"++show temps)

popTemp :: Int -> State Count ()
popTemp k = do (temps,labels) <- get
               put (temps-k,labels) 

newLabel :: State Count Label
newLabel = do (temps,labels) <- get
              put (temps,labels+1)
              return ("L"++show labels)

transExp :: (Exp, Table, Temp) -> State Count [Instr]
transExp (Num n, table, dest) = return [MOVEI dest n]

transExp (Var x, table, dest)
  = case Map.lookup x table of
      Just temp -> return [MOVE dest temp]
      Nothing -> error "invalid variable"

transExp (Break, table, dest) = return [BREAK]

transExp (IfThenElse cond e1 e2, table, dest) 
  = do ltrue <- newLabel
       lfalse <- newLabel
       lend <- newLabel
       code0 <- transCond(cond, table, ltrue, lfalse)
       code1 <- transExp (e1, table, "")
       code2 <- transExp (e2, table, "")
       return (code0 ++ [LABEL ltrue] ++ code1 ++
              [JUMP lend, LABEL lfalse] ++ code2 ++ [LABEL lend])

transExp (IfThen cond e1, table, dest) 
  = do ltrue <- newLabel
       lfalse <- newLabel
       code0 <- transCond(cond, table, ltrue, lfalse)
       code1 <- transExp (e1, table , "")
       return (code0 ++ [LABEL ltrue] ++ code1 ++ [LABEL lfalse])

transExp (OpBin op e1 e2, table, dest)
  = do case e1 of
         Num x -> do t2 <- newTemp
                     code2 <- transExp(e2, table, t2)
                     popTemp(1)
                     return (code2 ++ [OPI op dest t2 x])
         _ -> case e2 of
                Num x -> do t1 <- newTemp
                            code1 <- transExp(e1, table, t1)
                            popTemp(1)
                            return (code1 ++ [OPI op dest t1 x])
                _ -> do t1 <- newTemp
                        t2 <- newTemp
                        code1 <- transExp(e1, table, t1)
                        code2 <- transExp(e2, table, t2)
                        popTemp(2)
                        return (code1 ++ code2 ++ [OP op dest t1 t2])

transExp (OpRel op e1 e2, table, dest)
  = do label1 <- newLabel
       label2 <- newLabel
       code <- transCond(OpRel op e1 e2, table, label1, label2)
       return ([MOVEI dest 0] ++ code ++ [LABEL label1, MOVEI dest 1]
         ++ [LABEL label2])

transCond :: (Exp, Table, Label, Label) -> State Count [Instr]

transCond (cond, table, ltrue, lfalse)
  = case cond of
--      1 -> return [JUMP ltrue]
--      0 -> return [JUMP lfalse]
      
--      NegExp exp -> transCond(exp, table, lfalse, ltrue)
                
      OpRel op exp1 exp2 -> case op of
        And ->
          do lnext <- newLabel
             code1 <- transCond(exp1, table, lnext, lfalse)
             code2 <- transCond(exp2, table, ltrue, lfalse)
             return (code1 ++ [LABEL lnext] ++ code2)
           
        Or ->
          do lnext <- newLabel
             code1 <- transCond(exp1, table, ltrue, lnext)
             code2 <- transCond(exp1, table, ltrue, lfalse)
             return (code1 ++ [LABEL lnext] ++ code2)
           
        otherwise ->
          do temp1 <- newTemp
             temp2 <- newTemp
             popTemp(2)
             code1 <- transExp(exp1, table, temp1)
             code2 <- transExp(exp2, table, temp2)
             return (code1 ++ code2 ++ [COND temp1 op temp2 ltrue lfalse])

      exp -> do temp <- newTemp
                popTemp(1)
                code1 <- transExp(exp, table, temp)
                return (code1 ++ [COND temp Diff "0" ltrue lfalse])