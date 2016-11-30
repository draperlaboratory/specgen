{- |
   Module      :  Language.CSPM.SpecGen.UMLSyntax
   Description :  ADT and unwrapping/lookup functions for UML Statechart Diagrams,
   Copyright   :  Draper Laboratory
-}
module Language.CSPM.SpecGen.UMLSyntax where

import qualified Data.Set.Monad as S
import qualified Data.Map as M
import Data.List (intercalate)

-- Id represents identifiers in every structure
newtype Id = Id {getId :: String}
    deriving (Eq, Ord)

instance Show Id where
  show = getId

-- Node Id represents the identifier and a string with the
-- "plain English" name of the node
data NId = NId {nodeId :: Id, nodeName :: String}
    deriving (Eq, Ord)

instance Show NId where
  show n = "(N " ++ getId (nodeId n) ++ " " ++ nodeName n ++ ")"

-- Tuple definition of a StateChart
data StateChart = StateChart { root    :: Node,          -- Root node (Or node)
                               nIds    :: M.Map Id Node  -- Map from Ids to Nodes
                             }
  deriving (Show)


-- NODES --

-- Single state node
data Node = BasicNode NId Id (S.Set Edge)            -- Parent node, outgoing edges
          | AndNode   NId Id (S.Set Id) (S.Set Edge) -- Parent Node, substate nodes, outgoing edges
          | OrNode    NId (Maybe Id) (S.Set Id) Id   -- Parent (Nothing if Root)
                                                     -- substate nodes
                                                     -- Default substate node

-- Nodes identified by Id
instance Eq Node where
  n == m = getNodeId n == getNodeId m

instance Ord Node where
  compare n m = compare (getNodeId n) (getNodeId m)

instance Show Node where
  show = getNodeName

-- Map lookup function flipped arguments and helpful error message for failure
lookupFE :: (Ord k, Show k) => M.Map k a -> k -> a
lookupFE m i = maybe (error $ "Key " ++ show i ++ "Not In Map") id $ M.lookup i m

-- Send an Id to the corresponding node
toNode :: M.Map Id Node -> Id -> Node
toNode idMap = lookupFE idMap

-- Map set of Ids to nodes
toNodes :: M.Map Id Node -> S.Set Id -> S.Set Node
toNodes = S.map . toNode

-- NID projector
nId :: Node -> NId
nId (BasicNode nid _ _  ) = nid
nId (AndNode   nid _ _ _) = nid
nId (OrNode    nid _ _ _) = nid

-- Return the Id of a node
getNodeId :: Node -> Id
getNodeId = nodeId . nId

-- Return the 'plain English' name of a node
getNodeName :: Node -> String
getNodeName = nodeName . nId

-- Return substate nodes
children :: M.Map Id Node -> Node -> S.Set Node
children _     (BasicNode _ _  _) = S.empty
children idMap (AndNode _ _ ns _) = toNodes idMap ns
children idMap (OrNode  _ _ ns _) = toNodes idMap ns

-- Return Just parent node or Nothing if no parent exists (root)
parent :: M.Map Id Node -> Node -> Maybe Node
parent idMap (BasicNode _ p _  ) = Just $ toNode idMap p
parent idMap (AndNode   _ p _ _) = Just $ toNode idMap p
parent idMap (OrNode   _ mp _ _) = fmap (toNode idMap) mp

-- Returns all outgoing edges from a node from a set of edges
outEdges :: Node -> S.Set Edge
outEdges (BasicNode _ _ es) = es
outEdges (AndNode _ _ _ es) = es
outEdges (OrNode  _ _ _ _ ) = S.empty

-- Type indicating which type of node
data NType = Basic | And | Or
    deriving (Eq, Show)

-- Return type of node
nType :: Node -> NType
nType (BasicNode _ _ _) = Basic
nType (AndNode _ _ _ _) = And
nType (OrNode  _ _ _ _) = Or

-- Tests if a node has specified typeN

isBasic :: Node -> Bool
isBasic = (== Basic) . nType

isAnd :: Node -> Bool
isAnd = (== And) . nType

isOr :: Node -> Bool
isOr = (== Or) . nType

-- Return true if node has substates
composite :: Node -> Bool
composite (BasicNode   _ _ _) = False
composite (AndNode  _ _ cs _) = not $ S.null cs
composite (OrNode   _ _ cs _) = not $ S.null cs

-- Return true if node is the root
isRoot :: Node -> Bool
isRoot (OrNode _ Nothing _ _) = True
isRoot _                      = False

-- Return Just default substate node for Or nodes, or Nothing if And or Basic node
defaultN :: M.Map Id Node -> Node -> Maybe Node
defaultN idMap (OrNode _ _ _ n) = Just $ toNode idMap n
defaultN _ _                    = Nothing

-- EDGES --

-- Transition between two consistent sets of nodes
data Edge = Edge { eId    :: Id,            
                   source :: Id,  -- Nodes from which the edge transitions
                   target :: Id,  -- Nodes  to  which the edge transitions
                   event  :: Event,   -- Event triggering the transition
                   guard  :: Guard,   -- Condition for the transition to occur
                   action :: Action } -- Action performed when the transition occurs

-- Nodes identified by Id
instance Eq Edge where
  d == e = eId d == eId e

instance Ord Edge where
  compare d e = compare (eId d) (eId e)

instance Show Edge where
  show e = (show $ source e) ++ "-->" ++
           (show $ target e) ++ ": " ++
           (show $ event e) ++ "[" ++
           (show $ guard e) ++ "]/" ++
           (show $ action e)

-- Return source and target sets of Nodes from an edge
sourceN :: M.Map Id Node -> Edge -> Node
sourceN idMap = toNode idMap . source

targetN :: M.Map Id Node -> Edge -> Node
targetN idMap = toNode idMap . target

-- EVENTS --

data Event = Event {eventId :: Id}
           | Complete {getCompleteNode :: Node}
           | Anytime
    deriving (Eq, Ord, Show)

-- GUARDS --

-- Expressions representing boolean values for conditions on transitions occurring
data Guard = GLit Bool
           | GNot Guard
           | BBop GBBop Guard Guard
           | IBop GIBop Expr Expr
           | GIn  Node
           | Always
    deriving (Eq, Ord, Show)

-- Integer to Boolean binary operations
data GIBop = GEq
           | GNEq
           | GLt
           | GGt
           | GLEq
           | GGEq
    deriving (Eq, Ord, Show)

-- Boolean to Boolean binary operations
data GBBop = GOr
           | GAnd
    deriving (Eq, Ord, Show)

-- ACTIONS --

-- Expressions representing actions taken on transitions
data Action = Assign Var Expr
            | Seq Action Action
            | DoNothing
    deriving (Eq, Ord, Show)
            
-- VARS --

-- Variables
newtype Var = Var {varId :: Id}
    deriving (Eq, Ord, Show)

-- Values (here assumed to be only Int expressions)
data Expr = ELit Integer
          | EVar Var
          | ENeg Expr
          | EBop EIBop Expr Expr
          | EIf Guard Expr Expr
          | Tmr Node
          | GlobalTime
    deriving (Eq, Ord, Show)

data EIBop = EPlus
           | EMinus
           | EMult
           | EDiv
           | EMod
    deriving (Eq, Ord, Show)

prettyChart :: StateChart -> String
prettyChart (StateChart {root,nIds}) = prettyNode 0 root
  where
    indent :: Int -> String
    indent n = replicate (2*n) ' '

    indentEdges :: Int -> S.Set Edge -> String
    indentEdges i s =
      intercalate "\n" $ map (\e -> indent i ++ prettyEdge e) $ S.toList s

    prettyNode :: Int -> Node -> String
    prettyNode i (BasicNode (NId {nodeName}) _ edges) =
         (indent i) ++ nodeName ++ "(Basic):\n"
      ++ indentEdges (i+1) edges
    prettyNode i (AndNode (NId {nodeName}) _ subnodes edges) =
         (indent i) ++ nodeName ++ "(And):\n"
      ++ indentEdges (i+1) edges ++ "\n\n"
      ++ intercalate "\n\n"
           (map (prettyNode (i+1) . (toNode nIds)) (S.toList subnodes))
    prettyNode i (OrNode (NId {nodeName}) _ subnodes defaultSub) =
         (indent i) ++ nodeName
     ++ "(Or, default is " ++ (getNodeName $ toNode nIds defaultSub) ++ "):\n"
     ++ intercalate "\n\n"
          (map (prettyNode (i+1) . (toNode nIds)) (S.toList subnodes))

    prettyEdge :: Edge -> String
    prettyEdge (Edge {target,event,guard,action}) =
         "-->" ++ targetName ++ " "
      ++ prettyEvent event ++ " "
      ++ (if null guardString then "" else "[" ++ guardString ++ "] ")
      ++ (if null actionString then "" else "/" ++ actionString)
      where
        targetName = case M.lookup target nIds of
                       Just n -> getNodeName n
                       Nothing -> "UNKNOWN"

        guardString = prettyGuard guard

        actionString = prettyAction action
     
    prettyEvent :: Event -> String
    prettyEvent Anytime = ""
    prettyEvent (Complete n) = ":Complete(" ++ getNodeName n ++ ")"
    prettyEvent (Event eid)  = ":" ++ getId eid

    prettyGuard :: Guard -> String
    prettyGuard (GLit b) = show b
    prettyGuard (GNot g) = "~" ++ "(" ++ show g ++ ")"
    prettyGuard (BBop op g1 g2) =
      prettyGuard g1 ++ " " ++ prettyBBop op ++ " " ++ prettyGuard g2
    prettyGuard (IBop op e1 e2) =
      prettyExp e1 ++ " " ++ prettyIBop op ++ " " ++ prettyExp e2
    prettyGuard (GIn n) = "In(" ++ getNodeName n ++ ")"
    prettyGuard Always = ""

    prettyBBop :: GBBop -> String
    prettyBBop GOr = "||"
    prettyBBop GAnd = "&&"

    prettyIBop :: GIBop -> String
    prettyIBop GEq  = "=="
    prettyIBop GNEq = "/="
    prettyIBop GLt  = "<"
    prettyIBop GGt  = ">"
    prettyIBop GLEq = "<="
    prettyIBop GGEq = ">="

    prettyExp :: Expr -> String
    prettyExp (ELit i) = show i
    prettyExp (EVar v) = getId $ varId v
    prettyExp (ENeg e) = "(-" ++ show e ++ ")"
    prettyExp (EBop op e1 e2) =
      prettyExp e1 ++ " " ++ prettyEIBop op ++ " " ++ prettyExp e2
    prettyExp (EIf g e1 e2) =
      prettyGuard g ++ "?" ++ prettyExp e1 ++ ":" ++ prettyExp e2
    prettyExp (Tmr n) = "Tmr(" ++ getNodeName n ++ ")"
    prettyExp GlobalTime = "GlobalTime"

    prettyEIBop :: EIBop -> String
    prettyEIBop EPlus  = "+"
    prettyEIBop EMinus = "-"
    prettyEIBop EMult  = "*"
    prettyEIBop EDiv   = "/"
    prettyEIBop EMod   = "%"

    prettyAction :: Action -> String
    prettyAction (Assign v e) = (getId $ varId v) ++ " := " ++ prettyExp e
    prettyAction (Seq e1 e2)  = prettyAction e1 ++ "; " ++ prettyAction e2
    prettyAction DoNothing    = ""

      
