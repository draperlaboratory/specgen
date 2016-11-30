{- |
   Module      :  Language.CSPM.SpecGen.UMLFunctions
   Description :  Common Functions on UML StateCharts
   Copyright   :  Draper LaboratorY
-}

module Language.CSPM.SpecGen.UMLFunctions where

import Language.CSPM.SpecGen.UMLSyntax

import qualified Data.Set.Monad as S
import qualified Data.Map as M
import qualified Data.Foldable as F
import Data.Maybe (mapMaybe)
import Data.List (foldl')

-- StateChart Functions --

-- Returns all edges between nodes in sc
edges :: StateChart -> S.Set Edge
edges sc = deepSubEdges (nIds sc) $ root sc

-- Returns all nodes in sc
nodes :: StateChart -> S.Set Node
nodes sc = descendents (nIds sc) $ root sc

-- Information about the statechart extracted from examining the edges
data EdgeInfo = EdgeInfo {eiNamedEvents     :: S.Set Event,
                          eiCompletionNodes :: S.Set Node,
                          eiVars            :: S.Set Var,
                          eiTimerNodes      :: S.Set Node,
                          eiGlobalTimer     :: Bool}

emptyEdgeInfo :: EdgeInfo
emptyEdgeInfo = EdgeInfo {eiNamedEvents = S.empty,
                          eiCompletionNodes = S.empty,
                          eiVars = S.empty,
                          eiTimerNodes = S.empty,
                          eiGlobalTimer = False}

eiInsertNamedEvent :: Event -> EdgeInfo -> EdgeInfo
eiInsertNamedEvent ev ei =
  ei {eiNamedEvents = S.insert ev (eiNamedEvents ei)}

eiInsertCompletionNode :: Node -> EdgeInfo -> EdgeInfo
eiInsertCompletionNode n ei =
  ei {eiCompletionNodes = S.insert n (eiCompletionNodes ei)}

eiInsertVar :: Var -> EdgeInfo -> EdgeInfo
eiInsertVar v ei =
  ei {eiVars = S.insert v (eiVars ei)}

eiInsertTimerNode :: Node -> EdgeInfo -> EdgeInfo
eiInsertTimerNode n ei =
  ei {eiTimerNodes = S.insert n (eiTimerNodes ei)}

eiAddGlobalTimer :: EdgeInfo -> EdgeInfo
eiAddGlobalTimer ei = ei {eiGlobalTimer = True}

unionEdgeInfo :: EdgeInfo -> EdgeInfo -> EdgeInfo
unionEdgeInfo (EdgeInfo ne1 cn1 v1 tn1 gt1)
              (EdgeInfo ne2 cn2 v2 tn2 gt2) =
    EdgeInfo {eiNamedEvents     = S.union ne1 ne2,
              eiCompletionNodes = S.union cn1 cn2,
              eiVars            = S.union v1 v2,
              eiTimerNodes      = S.union tn1 tn2,
              eiGlobalTimer     = gt1 || gt2}

-- Returns all named events, completion nodes, vars, and timer nodes in a StateChart
-- As well as a boolean for whether or not a global timer is used
extractEdgeInfo :: StateChart -> EdgeInfo
extractEdgeInfo sc = foldl' extractFromEdge emptyEdgeInfo $ edges sc

-- Returns all named events, completion nodes, vars, and timer nodes in an Edge
-- As well as a boolean for whether or not a global timer is used
extractFromEdge :: EdgeInfo -> Edge -> EdgeInfo
extractFromEdge ei e =
  extractFromEvent (event e) $
    extractFromGuard (guard e) $
      extractFromAction (action e) ei

-- Returns the named event or completion node from an Event
extractFromEvent :: Event -> EdgeInfo -> EdgeInfo
extractFromEvent ev ei
  | isNamed ev      = eiInsertNamedEvent ev ei
  | isCompletion ev = eiInsertCompletionNode (getCompleteNode ev) ei
  | otherwise       = ei

-- Returns all Vars and timer nodes and a boolean for presence of a global timer in a Guard
extractFromGuard :: Guard -> EdgeInfo -> EdgeInfo
extractFromGuard (GNot g)       ei = extractFromGuard g ei
extractFromGuard (BBop _ g1 g2) ei = extractFromGuard g1 $ extractFromGuard g2 ei
extractFromGuard (IBop _ e1 e2) ei = extractFromExpr e1 $ extractFromExpr e2 ei
extractFromGuard (GIn n)        ei = extractFromExpr (Tmr n) ei
extractFromGuard Always         ei = ei
extractFromGuard (GLit _)       ei = ei

-- Returns all Vars and timer nodes and a boolean for presence of a global timer in an Action
extractFromAction :: Action -> EdgeInfo -> EdgeInfo
extractFromAction (Assign v e) ei = eiInsertVar v $ extractFromExpr e ei
extractFromAction (Seq a1 a2)  ei = extractFromAction a1 $ extractFromAction a2 ei
extractFromAction DoNothing    ei = ei

-- Returns all Vars and timer nodes and a boolean for presence of a global timer in an Expr
extractFromExpr :: Expr -> EdgeInfo -> EdgeInfo
extractFromExpr (EVar v)            ei = eiInsertVar v ei
extractFromExpr (ENeg e)            ei = extractFromExpr e ei
extractFromExpr (EBop _ e1 e2)      ei = extractFromExpr e1 $
                                           extractFromExpr e2 ei
extractFromExpr (EIf g e1 e2)       ei = extractFromGuard g $
                                          extractFromExpr e1 $
                                            extractFromExpr e2 ei
extractFromExpr (Tmr n) ei | isRoot n  = eiAddGlobalTimer ei
                           | otherwise = eiInsertTimerNode n ei
extractFromExpr GlobalTime          ei = eiAddGlobalTimer ei
extractFromExpr (ELit _)            ei = ei

-- Returns the set of all Events in a StateChart
events :: StateChart -> S.Set Event
events sc = S.union eiNamedEvents (S.map Complete eiCompletionNodes)
  where
    EdgeInfo {eiNamedEvents, eiCompletionNodes} = extractEdgeInfo sc

-- Returns all named Events in a StateChart
scNamedEvents :: StateChart -> S.Set Event
scNamedEvents sc = eiNamedEvents $ extractEdgeInfo sc

-- Returns all nodes whose completion events trigger transitions in a StateChart
-- Including the root, whose completion terminates the StateChart
scCompletionNodes :: StateChart -> S.Set Node
scCompletionNodes sc = S.insert (root sc) $ eiCompletionNodes $ extractEdgeInfo sc

-- Modifies a set of completion nodes to include the children of any And nodes
completeCompNodeSet :: M.Map Id Node -> S.Set Node -> S.Set Node
completeCompNodeSet idMap ns = F.foldMap toOrSet ns where
  toOrSet n | isOr    n = S.singleton n
            | isAnd   n = S.insert n $ children idMap n
            | isBasic n = S.empty
            | otherwise = error $ "Internal error: in completeCompNodeSet, "
                               ++ "node is neither Or, And or Basic."

-- Convenient composition of completeCompNodeSet and completionNodes
scCompletionNodes' :: StateChart -> S.Set Node
scCompletionNodes' sc = completeCompNodeSet (nIds sc) $ scCompletionNodes sc

-- XXX CJC: This is needed because the code seems to be generating "complete"
-- transitions referencing nodes that aren't completion nodes in the sense
-- defined immediately above, but which are "final" nodes.  I don't quite
-- understand the source of this problem, but for now I'm just changing the
-- generated code so that these transitions are allowed, which eliminates the
-- error.  Whether this results in a correct model is a mystery.
finalNodes :: StateChart -> S.Set Node
finalNodes sc = S.fromList $ mapMaybe finalParent $ M.elems $ nIds sc
  where
    finalParent n = case n of
                      BasicNode _ p es | null es -> M.lookup p (nIds sc)
                      _ -> Nothing

-- Returns all Vars in a StateChart
scVars :: StateChart -> S.Set Var
scVars = eiVars . extractEdgeInfo

-- Returns all nodes whose timers are used in a StateChart
scTmrNodes :: StateChart -> S.Set Node
scTmrNodes = eiTimerNodes . extractEdgeInfo

-- Modifies a set of timer nodes that does not include the root, replacing any or nodes with
-- their and node parent
completeTmrNodeSet :: M.Map Id Node -> S.Set Node -> S.Set Node
completeTmrNodeSet idMap ns = S.map toSubVNode ns where
  toSubVNode n | isOr n     = maybe (error "This set should not contain the root") id $ parent idMap n
               | otherwise  = n

-- Convenient composition of completeTmrNodeSet and tmrNodes
scTmrNodes' :: StateChart -> S.Set Node
scTmrNodes' sc = completeTmrNodeSet (nIds sc) $ scTmrNodes sc

-- Returns true if StateChart uses any node timers
scHasTmrs :: StateChart -> Bool
scHasTmrs = not . S.null . scTmrNodes'

-- Returns true if a StateChart uses a global timer
scHasGlobalTimer :: StateChart -> Bool
scHasGlobalTimer = eiGlobalTimer . extractEdgeInfo

-- Node Functions --

-- Returns all edges between substates of an Or Node (otherwise the empty set)
subEdges :: M.Map Id Node -> Node -> S.Set Edge
subEdges idMap n = outEdges =<< children idMap n

-- Returns all edges between substates in the chart rooted at n
deepSubEdges :: M.Map Id Node -> Node -> S.Set Edge
deepSubEdges idMap n = S.union (subEdges idMap n) $ deepSubEdges idMap =<< children idMap n

-- Return true if node is final (only basic nodes can be final, if no outgoing edges)
isFinal :: Node -> Bool
isFinal n = isBasic n && S.null (outEdges n)

-- Partial ordering on nodes by subtree membership
-- Return true if n >= m
ancestor :: M.Map Id Node -> Node -> Node -> Bool
ancestor idMap n m = m == n || maybe False (ancestor idMap n) (parent idMap m)

-- Return true if nodes are comparable in partial ordering
related :: M.Map Id Node -> Node -> Node -> Bool
related idMap n m = ancestor idMap n m || ancestor idMap m n

-- Return members of subtree rooted at a node
descendents :: M.Map Id Node -> Node -> S.Set Node
descendents idMap n = S.insert n . S.unions . map (descendents idMap) . S.toList . children idMap $ n

-- Return true if node has no Or node descendents (excluding itself)
hasNestedOrs :: M.Map Id Node -> Node -> Bool
hasNestedOrs idMap n = not . S.null . S.filter isOr . S.delete n . descendents idMap $ n

-- Returns true if n is an ancestor of every node in ms
commonAncestor :: M.Map Id Node -> S.Set Node -> Node -> Bool
commonAncestor idMap ms n = F.all (ancestor idMap n) ms

-- Tests if ns is 'connected' by ancestry relations
-- Returns true if ns contains a node that is a common ancestor of ns
connected :: M.Map Id Node -> S.Set Node -> Bool
connected idMap ns = F.any (commonAncestor idMap ns) ns

-- Ordering on related nodes. Only called on nodes known to be related
nodeCompare :: M.Map Id Node -> Node -> Node -> Ordering
nodeCompare idMap n m | n == m             = EQ
                      | ancestor idMap n m = GT
                      | otherwise          = LT

-- Returns all ancestors of a node
ancestors :: M.Map Id Node -> Node -> S.Set Node
ancestors idMap n = S.insert n . maybe S.empty (ancestors idMap) . parent idMap $ n

-- Returns Just the least common ancestor of ns, or Nothing if none exists
lca :: M.Map Id Node -> S.Set Node -> Maybe Node
lca idMap ns = let family = S.unions . map (ancestors idMap) $ S.toList ns in
  lca' idMap family ns

-- Returns the least common ancestor given a superset of nodes
lca' :: M.Map Id Node -> S.Set Node -> S.Set Node -> Maybe Node
lca' idMap allNodes ns = let cas = S.filter (commonAncestor idMap ns) allNodes in
  if (S.null cas) then Nothing else Just $ F.minimumBy (nodeCompare idMap) cas
    
-- Returns true if n and m are 'orthogonal'
-- n and m are orthogonal if not related and with an And node as their lca
orthogonal :: M.Map Id Node -> Node -> Node -> Bool
orthogonal idMap n m = not (related idMap n m) && maybe False isAnd (lca idMap $ S.fromList [n, m])

-- Returns true if n and m are either related or orthogonal
consistentPair :: M.Map Id Node -> Node -> Node -> Bool
consistentPair idMap n m = related idMap n m || orthogonal idMap n m

-- Returns true if a set of nodes is pairwise related or orthogonal
-- A consistent set of nodes represents a set of states that can be
-- simultaneously active in the execution of a statechart
consistent :: M.Map Id Node -> S.Set Node -> Bool
consistent idMap ns = F.all id $ do
  m <- ns
  n <- ns
  return $ consistentPair idMap n m

-- Type representing a valid set of nodes active in the execution of a statechart
-- In order to be a valid configuration, cNodes must be consistent and maximal
newtype Config = Config { cNodes :: S.Set Node }

-- Constructor for Configuration checking for consistency
-- Does not require a superset of nodes
newConfig :: (M.Map Id Node) -> S.Set Node -> Maybe Config
newConfig idMap ns | consistent idMap ns = Just $ Config ns
                   | otherwise           = Nothing

-- Returns true if ns is consistent and allNodes contains no nodes not in ns
-- that could be added to ns without losing consistency
maximal :: M.Map Id Node -> S.Set Node -> S.Set Node -> Bool
maximal idMap allNodes ns = consistent idMap ns && S.null (S.filter f allNodes) where
  f m = not (m `S.member` ns) && F.all (\n -> consistentPair idMap n m) ns

-- Constructor for Configuration checking for consistency and maximality
newMaxConfig :: M.Map Id Node -> S.Set Node -> S.Set Node -> Maybe Config
newMaxConfig idMap allNodes ns | maximal idMap allNodes ns = Just $ Config ns
                               | otherwise                 = Nothing

-- Returns true if node is an Or node and has a final substate in c
hasFinal :: M.Map Id Node -> Config -> Node -> Bool
hasFinal idMap c n | isOr n    = F.any isFinal . S.filter (`S.member` cNodes c) . children idMap $ n
                   | otherwise = False

-- Tests if state s is terminated in configuration c
-- Returns true if s is an Or node with a final substate in c
-- or if s is an And node whose children are all stopped in c
stopped :: M.Map Id Node -> Config -> Node -> Bool
stopped idMap c s = case nType s of
  Basic -> False
  Or    -> hasFinal idMap c s
  And   -> F.all (stopped idMap c) $ children idMap s

-- Edge Functions --

-- Returns the most nested Or node containing the nodes of both source and target
edgeScope :: M.Map Id Node -> Edge -> Maybe Node
edgeScope idMap e = let ns       = S.fromList [sourceN idMap e, targetN idMap e]
                        orFamily = S.filter isOr . S.unions . map (ancestors idMap) $ S.toList ns in
                    lca' idMap orFamily ns

-- Returns true if two edges are equal or their scopes are orthogonal
-- Indicates transitions that can safely occur simultaneously
consistentPairE :: M.Map Id Node -> Edge -> Edge -> Bool
consistentPairE idMap d e = (d == e ||) . maybe False id $ do
  sd <- edgeScope idMap d
  se <- edgeScope idMap e
  return $ orthogonal idMap sd se

-- Returns true if a set of edges is pairwise consistent
consistentE :: M.Map Id Node -> S.Set Edge -> Bool
consistentE idMap es = F.all id $ do
  d <- es
  e <- es
  return $ consistentPairE idMap d e

-- Returns the set of all edges in allEdges consistent with every edge in es
consTrans :: M.Map Id Node -> S.Set Edge -> S.Set Edge -> S.Set Edge
consTrans idMap allEdges es = S.filter (\e -> F.all (consistentPairE idMap e) es)
                                              allEdges

-- Event Functions --

-- Returns true if event is named, false if a completion event
-- gives an error on Anytime; use with caution
isNamed :: Event -> Bool
isNamed (Event _)    = True
isNamed _            = False

-- Returns true if event is a completion, false otherwise
isCompletion :: Event -> Bool
isCompletion (Complete _) = True
isCompletion _            = False
