module Transform where
import AST
import Data.Map
import Control.Monad.State
import Prelude hiding (id)
import Data.Set (Set, insert, empty)
import qualified Control.Applicative as Set

orToXor :: Exp -> Exp
orToXor (OR a b) = XOR (XOR a b) (AND a b)
orToXor e = e

andReduc :: Exp -> Exp
andReduc e@(AND e1 e2) =
    case (e1, e2) of
        (Var s1, Var s2) | s1 == s2  -> Var s1
        (NEG (Var s1), Var s2) | s1 == s2 -> Const False
        (Var s1, NEG (Var s2)) | s1 == s2 -> Const False
        (Const True, e2') -> e2'
        (e1', Const True) -> e1'
        (Const False, _) -> Const False
        (_, Const False) -> Const False
        _ -> e
andReduc e = e

orReduc :: Exp -> Exp
orReduc e@(OR e1 e2) =
    case (e1, e2) of
        (Var s1, Var s2) | s1 == s2  -> Var s1
        (NEG (Var s1), Var s2) | s1 == s2 -> Const True
        (Var s1, NEG (Var s2)) | s1 == s2 -> Const True
        (Const False, e2') -> e2'
        (e1', Const False) -> e1'
        (Const True, _) -> Const True
        (_, Const True) -> Const True
        _ -> e
orReduc e = e

xorReduc :: Exp -> Exp
xorReduc e@(XOR e1 e2) =
    case (e1, e2) of
        (Var s1, Var s2) | s1 == s2 -> Const False
        (NEG (Var s1), Var s2) | s1 == s2 -> Const True
        (Var s1, NEG (Var s2)) | s1 == s2 -> Const True
        (Const False, e2') -> e2'
        (e1', Const False) -> e1'
        (Const True, e2') -> NEG e2'
        (e1', Const True) -> NEG e1'
        _ -> e
xorReduc e = e

negReduc :: Exp -> Exp
negReduc (NEG (NEG e)) = negReduc e
negReduc (NEG (Const True)) = Const False
negReduc (NEG (Const False)) = Const True
negReduc e = e

transform :: (Exp -> Exp) -> Exp -> Exp
transform f e =
    case e of
        (OR e1 e2) -> f $ OR (transform f e1) (transform f e2)
        (AND e1 e2) -> f $ AND (transform f e1) (transform f e2)
        (XOR e1 e2) -> f $ XOR (transform f e1) (transform f e2)
        _ -> f e

type NodeID = Int

data Node =
      N_Var Int
    | N_And (NodeID, NodeID)
    | N_Xor (NodeID, NodeID)
    | N_Neg NodeID
    deriving (Show, Eq, Ord)

data DAGState = DAGState {
    nodes :: Map Node NodeID,
    incomingNodes :: Map Node (Set Node),
    leafs :: [Node],
    nxtid :: Int
} deriving (Show)

type DAGBuilder = State DAGState

nxtId :: DAGBuilder Int
nxtId = do
    dag <- get
    put $ dag {nxtid = nxtid dag + 1}
    pure (nxtid dag + 1)

addNode node = do
    dag <- get
    case Data.Map.lookup node (nodes dag) of
        Just _ -> pure ()
        Nothing -> do
            id <- nxtId
            dag <- get
            put $ dag {
                nodes = Data.Map.insert node id (nodes dag),
                incomingNodes = Data.Map.insert node Data.Set.empty (incomingNodes dag)
            }

getID node = do
    dag <- get
    case Data.Map.lookup node (nodes dag) of
        Just id -> pure id
        Nothing -> error "ID does not exist!"

addIncomingNode :: MonadState DAGState m => Node -> Node -> m ()
addIncomingNode fromNode toNode = do
    dag <- get
    put $ dag {incomingNodes = Data.Map.adjust (Data.Set.insert fromNode) toNode (incomingNodes dag)}

-- Do collapse
-- buildDAG :: Exp -> DAGState  
buildDAG exp = runState (buildDAG' exp)
                        (DAGState {nodes = Data.Map.empty, incomingNodes = Data.Map.empty, leafs = [], nxtid = 0})
    where
        buildInternalNode constructor exp1 exp2 = do
            node1 <- buildDAG' exp1
            node2 <- buildDAG' exp2
            id1 <- getID node1
            id2 <- getID node2
            if id1 == id2 then pure node1 
            else do
                let newNode = constructor (id1, id2)
                addNode newNode
                addIncomingNode newNode node1
                addIncomingNode newNode node2
                pure newNode

        buildDAG' exp' =
            case exp' of
                AND exp1 exp2 -> buildInternalNode N_And exp1 exp2
                XOR exp1 exp2 -> buildInternalNode N_Xor exp1 exp2
                e@(OR _ _) -> buildDAG' $ orToXor e
                Var i -> do
                    let newNode = N_Var i
                    addNode newNode
                    pure newNode
                _ -> undefined



