module TigerMips where

import Interm
import Parser
import Data.Map (Map)
import qualified Data.Map as Map

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