module StaticOptimizer where

import Gates

import Data.Set as Set

data QGateSet
  = SSingle Op Pos 
  | SC (Set Pos) Pos Op 
  | SCZ (Set Pos)
  deriving Show

qgatesToSet :: [QGate] -> [QGateSet]
qgatesToSet [] = []
qgatesToSet (h:t) = 
  case h of 
    Single op pos -> SSingle op pos : qgatesToSet t
    C pos_list pos op -> SC (fromList pos_list) pos op : qgatesToSet t
    CZ pos_list -> SCZ (fromList pos_list) : qgatesToSet t

qgatesToList :: [QGateSet] -> [QGate]
qgatesToList [] = []
qgatesToList (h:t) = 
  case h of 
    SSingle op pos -> Single op pos : qgatesToList t
    SC pos_set pos op -> C (toList pos_set) pos op : qgatesToList t
    SCZ pos_set -> CZ (toList pos_set) : qgatesToList t

mergeGates:: [QGateSet] -> [QGateSet]
mergeGates qg = undefined
  where 
    mergeGatesRec :: QGateSet -> [QGateSet] -> [QGateSet] -> [QGateSet]
    mergeGatesRec gate [] acc = gate : acc 
    mergeGatesRec gate1@(SC set1 pos1 op1) gates@(gate2 : rest) acc =
      case gate2 of 
        SC set2 pos2 op2 | pos1 == pos2 && op1 == op2 -> 
          mergeGatesRec (SC (Set.union set1 set2) pos1 op1) rest acc
        SC set2 _ _ | Set.member pos1 set2 -> gate1 : (acc ++ gates)
        SCZ set2 | Set.member pos1 set2 -> gate1 : (acc ++ gates)
        _ -> error "oh no"
    mergeGatesRec g gates acc = g : (gates ++ acc)
          
  

