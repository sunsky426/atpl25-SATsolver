module SpecEval.ANF where

import SpecEval.AST
import SpecEval.Gates
import Data.List (intercalate, union, nub)
import Data.Maybe (mapMaybe)

--- Type Definiations ---

type ANF = [ANFterm]
type ANFterm = [Atom]

--- conversion Exp -> ANF ---

elimOrNeg :: Exp -> Exp
elimOrNeg (Atom (Cst b)) = Atom $ Cst b
elimOrNeg (Atom (Var i)) = Atom $ Var i
elimOrNeg (NEG e) = XOR (elimOrNeg e) $ Atom $ Cst True
elimOrNeg (XOR e1 e2) = XOR (elimOrNeg e1) (elimOrNeg e2)
elimOrNeg (AND e1 e2) = AND (elimOrNeg e1) (elimOrNeg e2)
elimOrNeg (OR e1 e2) =
  let e1' = elimOrNeg e1
      e2' = elimOrNeg e2
   in XOR (XOR e1' e2') (AND e1' e2') -- equivalent to elimORwXOR

-- distribute AND across XOR.
distributeAnd :: Exp -> Exp
distributeAnd e =
  case e of
    AND e1 e2 -> distOne (distributeAnd e1) (distributeAnd e2)
    XOR e1 e2 -> XOR (distributeAnd e1) (distributeAnd e2)
    _ -> e
  where
    distOne e1 e2 =
      case (e1,e2) of
        (XOR a b, c) -> XOR (distOne a c) (distOne b c)
        (a, XOR b c) -> XOR (distOne a b) (distOne a c)
        _ -> AND e1 e2

makeAnf :: Exp -> Maybe ANF
makeAnf (XOR a b) = (++) <$> makeAnf a <*> makeAnf b
makeAnf e@(AND _ _) = (:[]) <$> makeAnfTerm e
makeAnf e@(Atom _) = (:[]) <$> makeAnfTerm e
makeAnf _ = Nothing

makeAnfTerm :: Exp -> Maybe ANFterm
makeAnfTerm (AND a b) = union <$> makeAnfTerm a <*> makeAnfTerm b
makeAnfTerm (Atom atom) = Just [atom]
makeAnfTerm _ = Nothing

simplifyAnf :: ANF -> ANF
simplifyAnf = mapMaybe simplifyAnfTerm -- TODO: also make so that "a xor a = Nothing"
  where 
    simplifyAnfTerm :: ANFterm -> Maybe ANFterm
    simplifyAnfTerm term = 
      if Cst False `elem` term 
        then Nothing 
        else Just $ filter (/= Cst True) $ nub term

exp2anf :: Exp -> Maybe ANF
exp2anf e = simplifyAnf <$> makeAnf (distributeAnd $ elimOrNeg e)

--- Conversion ANF -> oracle ---

anf2oracle :: ANF -> Program
anf2oracle = mapMaybe anfTerm2oracle
  where 
    anfTerm2oracle :: ANFterm -> Maybe Gate
    anfTerm2oracle [] = Nothing  
    -- empty list are 1's, resulting from removing one as identity on and. 
    -- However, in this case, it actually works out as "xor 1" correspondes to the global phase flip -I, which doesn't do anything on quantum systems.
    anfTerm2oracle l = MCZ <$> mapM extractVar l
    
    extractVar :: Atom -> Maybe Int
    extractVar (Var i) = Just i
    extractVar _ = Nothing

--- pretty printers ---

ppANF :: ANF -> String
ppANF anf = intercalate " +" (ppANFterm <$> anf)

ppANFterm :: ANFterm -> String
ppANFterm [] = "1"
ppANFterm l = concatMap ((' ':) . show) l
