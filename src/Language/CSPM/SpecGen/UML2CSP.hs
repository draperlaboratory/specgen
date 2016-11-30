{- |
   Module      :  Language.CSPM.SpecGen.UML2CSP
   Description :  Translated from UML ADT to CSPm ADT
   Copyright   :  Draper Laboratory
-}

module Language.CSPM.SpecGen.UML2CSP where

import Language.CSPM.SpecGen.UMLSyntax hiding (Id)
import qualified Language.CSPM.SpecGen.UMLSyntax as SC (Id)
import Language.CSPM.SpecGen.UMLFunctions
import Language.CSPM.Syntax

import qualified Data.Set.Monad as S
import qualified Data.Map as M
import Control.Monad.State.Lazy hiding (guard)
import Control.Monad.Writer.Lazy hiding (guard)
import Control.Monad.Reader hiding (guard)
import Data.List

-- Stateful computation returning an Id with the given string and an updating integer value
genId :: String -> State Int Id
genId s = do
  i <- get
  put $ succ i
  return $ Id (s, i)

-- Composition for 2 variable functions
comp2 :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
comp2 = (.) . (.)

-- Definitions for custom datatypes specific to each StateChart --

-- Writer and State type synonym for generating datatype maps and Ids
type MapWriter = WriterT (M.Map SC.Id Id) (State Int)

-- Extract stateful computation with label map from writing stateful computation
defineTypes :: StateChart -> State Int ([Definition], M.Map SC.Id Id)
defineTypes sc = runWriterT $ typeWriterState sc

-- Writing Stateful computation keeping track of integer counter
-- and writing to a map from SC Ids to CSPm Ids for type constructors
-- corresponding to the node, edge, event, or var with that Sc Id
typeWriterState :: StateChart -> MapWriter [Definition]
typeWriterState sc = sequence $
  [makeTriggerType sc,
   makeNodeType sc (not $ scHasTmrs sc),
   makeType "Variable" (map (\e -> (varId e, getId $ varId e)) (S.elems $ scVars sc)) True,
   makeEdgeType sc]

-- Creates a CSP datatype for a particular collection of UML identifiers, and
-- remembers what target identifiers are used for each UML ID.  The first arg is
-- the name of the created type.  The second is the list of UML IDs and "hints"
-- for prettier names.  The third arg indicates whether a dummy element should
-- be added to this type if there would otherwise be no constructors.
makeType :: String -> [(SC.Id, String)] -> Bool -> MapWriter Definition
makeType typeName sourceIDs addDummy = do
  idPairs <- mapM (\(sid,hint) -> do tid <- lift $ genId hint
                                     return (sid,tid))
                  sourceIDs
  tell $ M.fromList idPairs
  let constructorNames = if null sourceIDs && addDummy
                            then [Fixed $ typeName ++ "Empty"]
                            else map snd idPairs
  return $ DDataType (Fixed typeName) $ map (,[]) constructorNames

makeEdgeType :: StateChart -> MapWriter Definition
makeEdgeType sc = makeType "EdgeLabel" idPairs True
  where
    idPairs :: [(SC.Id,String)]
    idPairs = map (\e -> (eId e,prettyEdgeName e)) (S.elems $ edges sc)

    prettyEdgeName :: Edge -> String
    prettyEdgeName (Edge {source,target}) =
      (prettyNodeName sc sourceNode) ++ "___" ++ (prettyNodeName sc targetNode)
      where
        sourceNode = toNode (nIds sc) source
        targetNode = toNode (nIds sc) target

-- Generates nice names for nodes.  Guaranteed to be unique if both
--   1) no two nodes have the same names, as do their parents, all the way to the root
--   2) no immediate child of the root shares the name of the root
-- When a node's name is unique across the whole chart, we use that name.  When
-- it's not, we use a name based on its path.
prettyNodeName :: StateChart -> Node -> String
prettyNodeName sc node =
  -- There are two special cases to yield prettier names:
  --
  -- 1) Top-level nodes under the root.  The name of the root is typically ugly
  --    and not user-selected.  We don't include it in the names of these nodes.
  --
  -- 2) "Phantom" Or nodes.  For some reason, if you have an And node that isn't
  --    broken into multiple concurrent substates, EA gives it a single
  --    concurrent substate with a bad name like "EA_Region3" and puts all the
  --    children into that one state.  This may be to preserve the invariant
  --    that And nodes always have Or nodes as children (the "phantom" region is
  --    an Or node that separates the parent And node from its children, which
  --    are also And nodes if the parent doesn't have multiple concurrent
  --    substates).
  --
  --    First, if possible, we give this node a prettier name based on the name
  --    of its parent.
  --
  --    Second, we don't want the name of this phantom Or node to appear in the
  --    paths of all of its children, because it doesn't appear visually in the
  --    StateChart, so users won't expect it.  Thus, we check for the case where
  --    the parent of an And node is an Or node which is itself an only child
  --    with an ugly name, and leave the Or node out of the name in this case.
  --    This is an unfortunate heuristic.  A better solution might just be to
  --    delete the "phantom" Or node when constructing the StateChart, but I'm
  --    not sure if our translation relies on the invariant that the children of
  --    And nodes are Ors and vice versa so I'm going with the name hack for
  --    now.
  if S.member nodeName repeatedNames
     then
       -- In this branch we must use the full path of the node and so we check
       -- to see if the parent a phantom and omit it if so.
       case parent (nIds sc) node of
         Nothing -> nodeName
         Just dad ->
           case parent (nIds sc) dad of
             Nothing -> nodeName
             Just grandpa ->
               let pathName = prettyNodeName sc $
                     if sonIsPhantom grandpa dad then grandpa else dad
               in pathName ++ "_" ++ getNodeName node
     else
       -- In this branch we can use a "simple" name for the node.  We usully
       -- just use its name in the chart, but first we check to see if it's a
       -- phantom node and give it a prettier name of so.
       case parent (nIds sc) node of
         Nothing -> nodeName
         Just dad ->
           -- XXXX we should really check here that the new name is unique
           if sonIsPhantom dad node then (getNodeName dad ++ "Child") else nodeName
  where
    nodeName :: String
    nodeName = getNodeName node
    
    -- Call on grapndpa and dad.  Returns true if dad looks like a "Phantom" Or
    -- node in the sense described above.
    sonIsPhantom :: Node -> Node -> Bool
    sonIsPhantom (AndNode _ _ kids _) (OrNode nid _ _ _) =
      (S.size kids == 1) && uglyEAName nid
    sonIsPhantom _ _ = False

    uglyEAName :: NId -> Bool
    uglyEAName (NId {nodeName=nn}) = isPrefixOf "EA_Region" nn

    -- Node names that appear more than once in the chart
    repeatedNames :: S.Set String
    repeatedNames = snd $ M.foldr' accNode (S.empty,S.empty) $ nIds sc
      where
        -- first set is all names, second set is repeated names
        accNode :: Node -> (S.Set String, S.Set String)
                -> (S.Set String, S.Set String)
        accNode n (nms,rpts) =
          if S.member (getNodeName n) nms
             then (nms,S.insert nm rpts) else (S.insert nm nms,rpts)
          where
            nm = getNodeName n


-- Adds a constructor to a datatype - fails if input Definition isn't a datatype.
addConstructor :: Definition -> Id -> [Exp] -> Definition
addConstructor (DDataType i cs) nm args = DDataType i ((nm,args) : cs)
addConstructor e _ _ =
  error $ "Internal error: Non-datatype (" ++ show e
       ++ ") found where datatype expected."

-- Creates datetype for statechart nodes
-- Includes a dummy constructor if the StateChart has no timers for the timer node subtype
makeNodeType :: StateChart -> Bool -> MapWriter Definition
makeNodeType sc addTimerDummy = do
  nodeType <- makeType "NodeLabel" idPairs True
  return $ if addTimerDummy then addConstructor nodeType
                                     (Fixed "NodeLabelEmpty") []
                            else nodeType
  where
    idPairs :: [(SC.Id,String)]
    idPairs = map (\n -> (getNodeId n,prettyNodeName sc n)) $ M.elems $ nIds sc

-- Creates datatype for statechart events (or Triggers)
-- Includes a trigger for the completion of each Or node
makeTriggerType :: StateChart -> MapWriter Definition
makeTriggerType sc = do
    d <- makeType "Trigger" idPairs False
    case d of
      DDataType i cs -> return $
        DDataType i $ (Fixed "Completed", [fixedExp "CompLabel"]) : cs
      _ -> error $ "Internal error: makeType didn't return a DDataType "
                ++ "in makeTriggerType."
  where
    idPairs :: [(SC.Id,String)]
    idPairs = map (\e -> (eventId e, getId $ eventId e))
                  (S.elems $ scNamedEvents sc)

-- Creates subtypes for Named events, Or nodes, and And nodes
defineSubTypes :: M.Map SC.Id Id -> StateChart -> [Definition]
defineSubTypes typeLabels sc =
  concatMap (makeSubType typeLabels)
    [(S.map eventId $ scNamedEvents sc, "EventLabel", "Trigger"),
     (S.map getNodeId . S.filter isOr $ nodes sc, "ChartLabel", "NodeLabel"),
     (S.map getNodeId $ scTmrNodes' sc, "TmrLabel", "NodeLabel"),
     (S.map getNodeId compLabelNodes, "CompLabel", "NodeLabel")]
  where
    compLabelNodes :: S.Set Node
    compLabelNodes = S.union (finalNodes sc) (scCompletionNodes' sc)

-- Creates a subtype from the given set of Ids with the given label
--
-- If there are no constructors that belong in the subtype, we need to to a
-- little work to set up an empty set.
makeSubType :: M.Map SC.Id Id -> (S.Set SC.Id, String, String) -> [Definition]
makeSubType typeLabels (idSet, typeName, superTypeName) =
  if not (null idSet) then
    [DSubType (Fixed typeName) $ map (,[]) idList]
  else
    [DAnnot (Fixed typeName) (TSet $ TData $ Fixed superTypeName),
     DVar (PId $ Fixed typeName) (ESet [])]
  where
    idList = map (lookupFE typeLabels) $ S.toList idSet

-- Id Map Generation --


-- Generates map from each node to the Id for that node's process
makeProcMap :: StateChart -> String -> Node -> State Int (M.Map SC.Id Id)
makeProcMap sc suffix n = do
  let childNodes = S.toList $ children (nIds sc) n
  procLabels <- liftM M.unions $ mapM (makeProcMap sc suffix) childNodes
  procId <- genId $ (prettyNodeName sc n) ++ "__" ++ suffix
  return $ M.insert (getNodeId n) procId procLabels

-- Generates map from each variable Id to the Id for the temporary variable
-- that its value is read to in control processes,
-- and from each And/Basic node to the temporary variable for its timer value
makeVarTmrMap :: StateChart -> State Int (M.Map SC.Id Id)
makeVarTmrMap sc = do
  let varSCIds = map varId . S.toList $ scVars sc
  let tmrs = filter (not . isOr) . S.toList $ nodes sc
  varIds <- mapM (genId . getId) varSCIds
  tmrIds <- mapM (genId . ("tmr" ++) . (prettyNodeName sc)) tmrs
  return . M.fromList $ zip varSCIds varIds
                     ++ zip (map getNodeId tmrs) tmrIds


-- Map Reader Functions --


-- Type synonym for a tuple of Maps
type Maps = (M.Map SC.Id Node,
             M.Map SC.Id Id,
             M.Map SC.Id Id,
             M.Map SC.Id Id,
             M.Map SC.Id Id)

-- Type synonym for a Reader of Maps
type MapReader = Reader Maps

-- Template for reader simplifying functions involving SC Id to Node map
idLookUp :: (M.Map SC.Id Node -> a -> b) -> a -> MapReader b
idLookUp f a = do
  (idMap,_,_,_,_) <- ask
  return $ f idMap a

-- Reader simplifying toNode
getNode :: SC.Id -> MapReader Node
getNode = idLookUp toNode

-- Reader simplifying children
getChildren :: Node -> MapReader (S.Set Node)
getChildren = idLookUp children

-- Reader simplifying parent
getParent :: Node -> MapReader (Maybe Node)
getParent = idLookUp parent

-- Reader simplifying fromJust . parent (throws error if called on root)
getParent' :: Node -> MapReader Node
getParent' = idLookUp $ maybe (error "Root has no Parent") id `comp2` parent

-- Reader simplifying default (throws error if not called on Or node)
getDefaultN :: Node -> MapReader Node
getDefaultN = idLookUp $ maybe (error "Only Or nodes have default") id `comp2` defaultN

-- Reader simplifying subEdges
getSubEdges :: Node -> MapReader (S.Set Edge)
getSubEdges = idLookUp subEdges

-- Reader simplifying descendents
getDescendents :: Node -> MapReader (S.Set Node)
getDescendents = idLookUp descendents

-- Reader simplifying hasNestedOrs
getHasNestedOrs :: Node -> MapReader Bool
getHasNestedOrs = idLookUp hasNestedOrs

-- Reader simplifying completeTmrNodeSet
getCompleteTmrNodeSet :: S.Set Node -> MapReader (S.Set Node)
getCompleteTmrNodeSet = idLookUp completeTmrNodeSet

-- Reader function returning the type Id corresponding to the SC Id
getType :: SC.Id -> MapReader Id
getType i = do
  (_,typeLabels,_,_,_) <- ask
  return $ lookupFE typeLabels i

-- Reader function returning the type Id of an Or node
getNodeType :: Node -> MapReader Id
getNodeType = getType . getNodeId

-- Reader function returning the type Id of an Edge
getEdgeType :: Edge -> MapReader Id
getEdgeType = getType . eId

-- Reader function returning the type Id of a trigger
getTrigType :: Event -> MapReader Id
getTrigType = getType . eventId

-- Reader function returning the type Id of a Var
getVarType :: Var -> MapReader Id
getVarType = getType . varId

-- Reader function returning the process Id of a node
getProcId :: Node -> MapReader Id
getProcId n = do
  (_,_,procLabels,_,_) <- ask
  return . lookupFE procLabels $ getNodeId n

-- Reader function returning Id of the structural process containing
-- an And or Basic node or the control process of an Or node
getStrCtrId :: Node -> MapReader Id
getStrCtrId n = do
  (_,_,_,strCtrLabels,_) <- ask
  return . lookupFE strCtrLabels $ getNodeId n

-- Reader function returning the Id of the temporary variable for a Var
getVarId :: Var -> MapReader Id
getVarId v = do
  (_,_,_,_,varTmrLabels) <- ask
  return . lookupFE varTmrLabels $ varId v

-- Reader function returning the Id of the temporary variable for a timer at a node
getTmrId :: Node -> MapReader Id
getTmrId n = do 
  (_,_,_,_,varTmrLabels) <- ask
  return . lookupFE varTmrLabels $ getNodeId n

-- Reader function returning Just the Id of the event if a named Event
-- or the Id of the node of a completion Event
-- or Nothing if Anytime
idFromEvent :: Event -> MapReader (Maybe Id)
idFromEvent ev = case ev of
    Event i    -> liftM Just . getTrigType $ Event i
    Complete n -> liftM Just $ getNodeType n
    Anytime    -> return Nothing


-- Converting expressions, guards, and actions into CSPm --


-- Returns the pair of lists containing the Vars and Nodes with timers used by an Or node
-- containing the given edges, and a boolean for whether the global time is used
getVarsTmrs :: S.Set Edge -> MapReader (S.Set Var, S.Set Node, Bool)
getVarsTmrs es = do
  tnodes <- getCompleteTmrNodeSet eiTimerNodes
  return (eiVars, tnodes, eiGlobalTimer)
  where
    EdgeInfo {eiVars, eiTimerNodes, eiGlobalTimer} =
        foldl' extractFromEdge emptyEdgeInfo (S.toList es)
  
-- Converts from Int to Int binary operations in UML syntax to CSPm
eBopConvert :: EIBop -> BinOp
eBopConvert EPlus  = BPlus
eBopConvert EMinus = BMinus
eBopConvert EMult  = BTimes
eBopConvert EDiv   = BDiv
eBopConvert EMod   = BMod

-- Reader generating an Exp in CSPm from an Expr in UML syntax
exprToExp :: Expr -> MapReader Exp
exprToExp (ELit i)            = return . EConst $ CInt i
exprToExp (EVar v)            = liftM EId $ getVarId v
exprToExp (ENeg e)            = liftM (EUOp UNeg) $ exprToExp e
exprToExp (EBop b e1 e2)      = do
  exp1 <- exprToExp e1
  exp2 <- exprToExp e2
  return $ EBinOp exp1 (eBopConvert b) exp2
exprToExp (EIf g e1 e2)       = do
  gexp <- guardToExp g
  exp1 <- exprToExp e1
  exp2 <- exprToExp e2
  return $ EIfThenElse gexp exp1 exp2
exprToExp (Tmr n) | isRoot n  = exprToExp GlobalTime
                  | isOr   n  = liftM EId $ getTmrId =<< getParent' n
                  | otherwise = liftM EId $ getTmrId n
exprToExp GlobalTime          = return $ EId globalTimeId

-- Converts from Bool to Bool binary operations in UML syntax to CSPm
bBopConvert :: GBBop -> BinOp
bBopConvert GOr  = BOr
bBopConvert GAnd = BAnd

-- Converts from Int to Bool binary operations in UML syntax to CSPm
iBopConvert :: GIBop -> BinOp
iBopConvert GEq  = BEq
iBopConvert GNEq = BNeq
iBopConvert GLt  = BLt
iBopConvert GGt  = BGt
iBopConvert GLEq = BLeq
iBopConvert GGEq = BGeq

-- Reader generating a boolean expression in CSPm from a Guard
guardToExp :: Guard -> MapReader Exp
guardToExp (GLit b)         = return . EConst $ CBool b
guardToExp (GNot g)         = liftM (EUOp UNot) $ guardToExp g
guardToExp (BBop bop g1 g2) = do
  ge1 <- guardToExp g1
  ge2 <- guardToExp g2
  return $ EBinOp ge1 (bBopConvert bop) ge2
guardToExp (IBop bop e1 e2) = do
  ee1 <- exprToExp e1
  ee2 <- exprToExp e2
  return $ EBinOp ee1 (iBopConvert bop) ee2
guardToExp (GIn n)          = guardToExp $ IBop GNEq (Tmr n) (ELit (-1))
guardToExp Always           = return . EConst $ CBool True

-- Reader generating a list of write statements from an Action
actionToExps :: Action -> MapReader [(Exp, [CommField])]
actionToExps (Assign v e) = do
  varType <- getVarType v
  eexp     <- exprToExp e
  return [(writeE, [dotCon varType, CFPlain eexp])]
actionToExps (Seq a1 a2)  = do
  al1 <- actionToExps a1
  al2 <- actionToExps a2
  return $ al1 ++ al2
actionToExps DoNothing    = return []


-- Convenience functions for CSPm syntax


-- Produces from a list of processes the process of external choice between them
-- STOP is an identity for external choice
listExtChoice :: [Exp] -> Exp
listExtChoice = foldr EExtChoice (EConst CStop)

-- Produces from a list of events and their fields, and a process,
-- the process of successive prefixing of the list to the given process
listPrefix :: Exp -> [(Exp, [CommField])] -> Exp
listPrefix = foldr $ uncurry EPrefix

-- Convenient composition of plain dot and Exp constructor for custom datatype
-- constructors with no arguments
dotCon :: Id -> CommField
dotCon = CFPlain . EId

-- Convenient composition of input ? and pattern constructor for basic variables
qMarkVar :: Id -> CommField
qMarkVar = CFInput . PId

-- Convenient composition of Exp constructor and fixed Id constructor
fixedExp :: String -> Exp
fixedExp = EId . Fixed

-- Convenient composition of Pat constructor and fixed Id constructor
fixedPat :: String -> Pat
fixedPat = PId . Fixed


-- Functions for expressing relevant CSPm Processes --


-- Expressions for channels for convenience
completeE, isCompleteE, stepE, tockE, readE, writeE :: Exp
transitionE, proceedE, stableE, controlRootE, getGlobalTimeE :: Exp
getInputE, usedEventE, getTimeE, startTimeE, endTimeE :: Exp
[completeE, isCompleteE, stepE, tockE, readE, writeE,
 transitionE, proceedE, stableE, controlRootE, getGlobalTimeE,
 getInputE, usedEventE, getTimeE, startTimeE, endTimeE] =
  map fixedExp ["complete", "isComplete", "step", "tock", "read", "write",
                "transition", "proceed", "stable", "ControlRoot", "getGlobalTime",
                "getInput", "usedEvent", "getTime", "startTime", "endTime"]

-- Input variables for triggers, global time
inputId, globalTimeId :: Id
[inputId, globalTimeId] = map Fixed ["input", "globalTime"]

-- Produces the process 'step -> P' from P
stepTo :: Id -> Exp
stepTo p = EPrefix stepE [] (EId p)

-- Produces the process for a transition in the structural Or node processes
transOut :: S.Set Node -> Node -> Edge -> MapReader Exp
transOut tNodes scope e = do
  scopeType  <- getNodeType scope
  edgeType   <- getEdgeType e
  sourceNode <- getNode $ source e
  sourceType <- getNodeType sourceNode
  targetProc <- getStrCtrId =<< (getNode $ target e)
  nestedTmrs <- liftM (S.intersection tNodes) $ getDescendents sourceNode
  tmrIds     <- mapM getNodeType $ S.toList nestedTmrs
  return $ listPrefix (EId targetProc) $ [(transitionE, map dotCon [scopeType, edgeType]),
                                          (stepE, [])] ++
                                         map endTmr tmrIds ++
                                         (if (S.member sourceNode tNodes)
                                          then [endTmr sourceType]
                                          else []) where
                                           endTmr i = (endTimeE, [dotCon i])

-- Produces the definition of the structural process containing an And or Basic node
-- Includes turning timer on and off if Node is in set of timer nodes
defStrProc :: S.Set Node -> Node -> MapReader Definition
defStrProc tNodes n = do
  nodeType <- getNodeType n
  strProc  <- getStrCtrId n
  mainProc <- getProcId n
  p        <- getParent' n
  exits    <- mapM (transOut tNodes p) . S.toList $ outEdges n
  return $ DVar (PId strProc)
                (EInterrupt (if (S.member n tNodes)
                             then (EPrefix startTimeE [dotCon nodeType] (EId mainProc))
                             else (EId mainProc))
                            (listExtChoice exits))

-- Produces the definition of the main process of an Or node
defOr :: Node -> MapReader Definition
defOr n = do
  mainProc  <- getProcId n
  ctrProc   <- getStrCtrId n
  nodeType  <- getNodeType n
  initial   <- getStrCtrId =<< getDefaultN n
  let rootCheck = isRoot n
  return $ DVar (PId mainProc)
                (EGenParallel (if rootCheck then controlRootE else (EId ctrProc))
                              (EEnumSet $ [stepE,
                                           EDot transitionE [EId nodeType],
                                           EDot proceedE    [EId nodeType],
                                           EDot stableE     [EId nodeType]] ++
                                          (if rootCheck
                                           then [EDot completeE [EId nodeType]]
                                           else []))
                              (EId initial))

-- Returns the Exp for an event (assumed not Anytime)
-- given an Id for either the event ofr completion node 
eventExp :: Event -> Id -> Exp
eventExp ev i | isNamed ev = EId i                                  -- i is Id for named Event
              | otherwise  = EDot (EId $ Fixed "Completed") [EId i] -- i is Id for completion node

-- Produces the process for a transition in the control process of an Or node
transCtr :: Node -> Edge -> MapReader Exp
transCtr scope e = do
  triggerOrNode <- idFromEvent $ event e -- Maybe Id for either named Event or completion node
  guardExp      <- guardToExp $ guard e
  scopeType     <- getNodeType scope
  edgeType      <- getEdgeType e
  actionExps    <- actionToExps $ action e
  ctrProc       <- getStrCtrId scope
  return $ EGuarded (EBinOp (maybe (EConst $ CBool True)
                                   (\t -> EApp (fixedExp "member") [eventExp (event e) t, EId inputId])
                                   triggerOrNode)
                            BAnd guardExp)
                    (listPrefix (stepTo ctrProc)
                                ((transitionE, map dotCon [scopeType, edgeType]) : actionExps ++
                                 maybe [] (\t -> [(usedEventE, [CFPlain $ eventExp (event e) t])])
                                          triggerOrNode))

-- Produces the process for triggering proceed in the control process of an Or node
proceedProc :: Maybe Node -> Node -> MapReader Exp
proceedProc mp n = do
  nodeType  <- getNodeType n
  ctrProc   <- getStrCtrId n
  stabilize <- stableProc mp n
  return $ EPrefix proceedE [dotCon nodeType, CFPlain $ EId inputId]
                   (EExtChoice (stepTo ctrProc)
                               (EPrefix stableE [dotCon nodeType] stabilize))

-- Produces the process for what to do when there are no possible transitions
-- Accounts for special case of root node
stableProc :: Maybe Node -> Node -> MapReader Exp
stableProc Nothing  _ = return controlRootE
stableProc (Just p) n = do
  higherType <- getNodeType p
  ctrProc    <- getStrCtrId n
  return $ EExtChoice (EPrefix stableE [dotCon higherType]
                               (EExtChoice (EId ctrProc) (stepTo ctrProc)))
                      (stepTo ctrProc)

-- Produces the definition of the control process of an Or node
-- Accounts for special case of the root
defCtr :: Maybe Node -> Node -> MapReader Definition
defCtr mp n = do
  (usedVarSet, tmrNodeSet, hasGlobal) <- getVarsTmrs =<< getSubEdges n
  let (usedVars, tmrNodes) = (S.toList usedVarSet, S.toList tmrNodeSet)
  ctrProc    <- getStrCtrId n
  higherType <- maybe (return $ Fixed "") getNodeType mp
  varTypes   <- mapM getVarType usedVars
  varIds     <- mapM getVarId   usedVars
  tmrTypes   <- mapM getNodeType tmrNodes
  tmrIds     <- mapM getTmrId    tmrNodes
  edgeList   <- liftM S.toList $ getSubEdges n
  stabilize  <- stableProc mp n
  proceed    <- proceedProc mp n
  transProcs <- mapM (transCtr n) edgeList
  hasNested  <- getHasNestedOrs n
  let rootCheck = isRoot n
  return $ DVar (PId ctrProc)
                (listPrefix (listExtChoice (stabilize :
                                            (if hasNested then [proceed] else []) ++
                                            transProcs))
                            ((if rootCheck
                              then (getInputE, [qMarkVar inputId])
                              else (proceedE, [dotCon higherType, qMarkVar inputId])) :
                             (if hasGlobal
                              then [(getGlobalTimeE, [qMarkVar globalTimeId])]
                              else []) ++
                             map (readWith readE)    (zip varTypes varIds) ++
                             map (readWith getTimeE) (zip tmrTypes tmrIds))) where
    readWith readerExp (vt,vi) = (readerExp, [dotCon vt, qMarkVar vi])

-- Produces the Exp of the parallelized child processes of an And node
parallelOrProcs :: Id -> [Exp] -> Exp
parallelOrProcs higherType childExps =
  foldr1 (flip EGenParallel (EEnumSet [stepE,
                                       EDot proceedE [EId higherType],
                                       EDot stableE  [EId higherType]]))
         childExps

-- Produces the definition of the process of an And node
defAnd :: S.Set Node -> Node -> Node -> MapReader Definition
defAnd cNodes p n = do
  mainProc   <- getProcId n
  nodeType   <- getNodeType n
  let nodeTypeExp = EId nodeType
  higherType <- getNodeType p
  childList  <- liftM S.toList $ getChildren n
  childExps  <- mapM (liftM EId . getProcId) childList
  return $ DVar (PId mainProc)
                (if (S.member n cNodes)
                 then ((EGenParallel (ERename (EGenParallel (parallelOrProcs higherType childExps)
                                                            (EEnumSet [completeE])
                                                            (EApp (fixedExp "AndCompletes")
                                                                  [nodeTypeExp, ESet []]))
                                              [(isCompleteE, EDot completeE [nodeTypeExp])])
                                     (ESet [EDot completeE [nodeTypeExp]])
                                     (EPrefix completeE [dotCon nodeType] (EConst CStop))))
                 else (parallelOrProcs higherType childExps))

-- Generates the map from each And node label to the set of labels of its children
makeAndChildMap :: M.Map SC.Id Id -> StateChart -> Definition
makeAndChildMap typeLabels sc = let andNodes   = filter isAnd . S.toList $ scCompletionNodes sc
                                    andExps    = map (EId . (lookupFE typeLabels) . getNodeId)  andNodes
                                    childNodes = map (S.toList . children (nIds sc)) andNodes
                                    childExps  = map (map (EId . (lookupFE typeLabels) . getNodeId))
                                                     childNodes
                                in DVar (fixedPat "andChildren")
                                        (EMap $ zip andExps (map ESet childExps))

-- Produces the definition of the process of a Basic node
defBasic :: Node -> Node -> MapReader Definition
defBasic p n = do
  mainProc   <- getProcId n
  higherType <- getNodeType p
  return $ DVar (PId mainProc) (if (isFinal n)
                                then (EApp (fixedExp "Finalize__") [EId higherType])
                                else (fixedExp "Basic"))

-- Produces the definition of the superstep generating ControlRoot process
makeControlRoot :: M.Map SC.Id Id -> SC.Id -> Definition
makeControlRoot typeLabels rootId = DVar (fixedPat "ControlRoot")
                                         (EExtChoice (EPrefix tockE [CFInput PWildCard]
                                                              (fixedExp "Controller"))
                                                     (EPrefix completeE
                                                              [dotCon $ lookupFE typeLabels rootId]
                                                              (EConst CSkip)))


-- StateChart Traversal --


-- Generates processes representing the StateChart rooted at a node
-- Using the given maps to dereference Nodes and track CSPm Ids
scProcs :: Maps -> S.Set Node -> S.Set Node -> Node -> [Definition]
scProcs maps cNodes tNodes r = runReader (orProcs cNodes tNodes Nothing r) maps where

-- Reader generating definitions of all nodes contained within an Or node
orProcs :: S.Set Node -> S.Set Node -> Maybe Node -> Node -> MapReader [Definition]
orProcs cNodes tNodes mp n = do
  orMain    <- defOr n
  orCtr     <- defCtr mp n
  childList <- liftM S.toList $ getChildren n
  orStructs <- mapM (defStrProc tNodes) childList
  childDefs <- liftM concat . sequence $ map (nodeProcs cNodes tNodes n) childList
  return $ orMain : orCtr : orStructs ++ childDefs

-- Reader generating definitions of all nodes contained within a node
nodeProcs :: S.Set Node -> S.Set Node -> Node -> Node -> MapReader [Definition]
nodeProcs cNodes tNodes p n
  | isBasic n = liftM return $ defBasic p n
  | isAnd   n = do
      andMain <- defAnd cNodes p n
      childList <- liftM S.toList $ getChildren n
      childDefs <- liftM concat $ mapM (nodeProcs cNodes tNodes p) childList
      return $ andMain : childDefs
  | isOr n = orProcs cNodes tNodes (Just p) n
  | otherwise = error $ "Error in nodeProcs: node " ++ show n
                     ++ " is neither Basic, And, nor Or."
              

-- Top level conversion from StateChart syntax into CSPm --


-- Returns a pair of FDRMods, the first for the custom datatypes,
-- second for the processes representing the statechart
toCSP :: StateChart -> FDRMod
toCSP sc = evalState convertWithIds 0
  where
    convertWithIds = do
      let rootId = getNodeId $ root sc
      (typeDefs, typeLabels) <- defineTypes sc
      procLabels <- makeProcMap sc "PROC" (root sc)
      strCtrLabels <- makeProcMap sc "CONTROL" (root sc) -- for structural/control processes
      varLabels <- makeVarTmrMap sc
      return . FDRMod $ DInclude "sgbase.csp" :
                        makeAndChildMap typeLabels sc :
                        makeControlRoot typeLabels rootId :
                        typeDefs ++
                        defineSubTypes typeLabels sc ++
                        scProcs (nIds sc,
                                 typeLabels,
                                 M.insert rootId (Fixed "Root") procLabels,
                                 M.insert rootId (Fixed "Controller") strCtrLabels,
                                 varLabels)
                                (scCompletionNodes' sc)
                                (scTmrNodes' sc)
                                (root sc)
                              
