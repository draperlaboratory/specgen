{- |
   Module      :  Language.CSPM.SpecGen.XML2UML
   Description :  Converts from internal XML representation to internal StateChart representation
   Copyright   :  Draper Laboratory
-}

module Language.CSPM.SpecGen.XML2UML where

import Language.CSPM.SpecGen.UMLSyntax
import Language.CSPM.SpecGen.UMLFunctions
import Language.CSPM.SpecGen.UMLParsers
import qualified Data.Set.Monad as S
import qualified Data.Map as M
import Text.XML.Light.Proc hiding (lookupAttr, findAttr)
import Text.XML.Light.Types

import Data.Maybe
import Data.List
import Data.Function
import Control.Monad.Writer.Lazy
import Control.Monad.Reader

-- Helper functions to find attributes in Elements
lookupAttr :: String -> [Attr] -> Maybe String
lookupAttr s = lookupAttrBy (\q -> s == qName q)

findAttr :: String -> Element -> Maybe String
findAttr s = findAttrBy (\q -> s == qName q)

-- Various tests on Elements

-- Subvertices represent And/Basic nodes
isSubV :: Element -> Bool
isSubV q = let s = qName $ elName q in
           s == "subvertex"

-- Regions represent Or nodes
isRegion :: Element -> Bool
isRegion q = let s = qName $ elName q in
             s == "region"

-- Edge elements
isEdge :: Element -> Bool
isEdge q = let s = qName $ elName q in
           s == "transition"

-- Event elements
isEvent :: Element -> Bool
isEvent q = let s = qName $ elName q in
           s == "trigger"

-- Guard elements
isGuard :: Element -> Bool
isGuard q = let s = qName $ elName q in
           s == "guard"

-- Guard Specification elements
isGuardSpec :: Element -> Bool
isGuardSpec q = let s = qName $ elName q in
           s == "specification"

-- Action elements
isAction :: Element -> Bool
isAction q = let s = qName $ elName q in
           s == "effect"

-- Get the attribute of a descendent by name
getDescAttr :: String -> Element -> Maybe String
getDescAttr attr e = filterElement (isJust . findAttr attr) e >>= findAttr attr

-- Helper functions to get certain types of sub-nodes

-- Sub And/Basic nodes
getSubV :: Element -> [Element]
getSubV = filterChildren isSubV

-- Outgoing edges
getEdges :: Element -> [Element]
getEdges = filterChildren isEdge

-- Sub Or nodes
getSubRegions :: Element -> [Element]
getSubRegions = filterChildren isRegion

-- Helper functions to get a Maybe sub-node for optional transition properties

-- Event element
getEvent :: Element -> Maybe Element
getEvent = filterElement isEvent

-- Guard Specification element
getGuardSpec :: Element -> Maybe Element
getGuardSpec = (filterElement isGuardSpec =<<) . filterElement isGuard

-- Action element
getAction :: Element -> Maybe Element
getAction = filterElement isAction

-- Build an Edge from an xml edge
-- Looks up Event eMap and parses Guard and Action from strings
mkEdge :: M.Map String Event -> Element -> Edge
mkEdge eMap e = let attrs        = elAttribs e
                    transId      = lookupAttr "id" attrs
                    sourceId     = lookupAttr "source" attrs
                    targetId     = lookupAttr "target" attrs
                    eventString  = findAttr "idref" =<< getEvent e
                    guardString  = findAttr "body"  =<< getGuardSpec e
                    actionString = findAttr "body"  =<< getAction e
                in case (transId, sourceId, targetId) of
                    (Just tr, Just so, Just ta) ->
                        Edge (Id tr)
                             (Id so)
                             (Id ta)
                             (case eventString of
                                Nothing -> Anytime
                                Just s  ->
                                  case M.lookup s eMap of
                                    Nothing -> error $ "Missing event for id " ++ s
                                    Just ev -> ev)
                             (maybe Always parseGuard guardString)
                             (maybe DoNothing parseAction actionString)
                    _ -> error "mkEdge: element not a transition!"

-- Create the data information (id, name) of an Element
mkNId :: Element -> NId
mkNId e = case (findAttr "id" e, findAttr "name" e) of
                (Just n, Just name) ->
                    NId (Id n) name
                _ -> error "mkData: not a well structured node!"

-- Returns "Initial" subvertex of an Or Node from a list of children
-- "Initial" and "Final" nodes are specified in EA
getInitial :: [Node] -> Maybe Node
getInitial [] = Nothing
getInitial (n:ns) = Just $ fromMaybe n $ find ((== "Initial") . getNodeName) (n:ns)

-- Convert from a StateChart rooted at an XNode to a StateChart in UML Syntax

-- Initial conversion to StateChart
mkSC :: M.Map String Event -> Element -> StateChart
mkSC eMap eroot = StateChart {root,nIds}
  where
    (root,nIds) = runWriter (mkOrNode eMap Nothing eroot)

-- Writer type synonym for generating a statechart
type MapWriter = Writer (M.Map Id Node)

-- Chooses between Basic or And Node
mkSub :: M.Map String Event -> Maybe Id -> [Edge] -> Element -> MapWriter Node
mkSub eMap mpid es e | null (filterChildren isRegion e) = mkBasicNode mpid es e
                     | otherwise                        = mkAndNode   eMap mpid es e

-- Basic Nodes
mkBasicNode :: Maybe Id -> [Edge] -> Element -> MapWriter Node
mkBasicNode (Just pid) es e = let nid     = mkNId e
                                  edgeSet = S.fromList . filter ((== nodeId nid) . source) $ es
                                  n       = BasicNode nid pid edgeSet
                              in do
                                insertNPair n
                                return n
mkBasicNode _ _ _           = error "Basic Node must have a parent"

-- And Nodes
mkAndNode :: M.Map String Event -> Maybe Id -> [Edge] -> Element -> MapWriter Node
mkAndNode eMap (Just pid) es e = let nid     = mkNId e
                                     regions = getSubRegions e
                                     edgeSet = S.fromList . filter ((== nodeId nid) . source) $ es
                                 in do
                                   childList <- mapM (mkOrNode eMap (Just $ nodeId nid)) regions
                                   let n = AndNode nid
                                                   pid
                                                   (S.fromList . map getNodeId $ childList)
                                                   edgeSet
                                   insertNPair n
                                   return n
mkAndNode _ _ _ _              = error "And Node must have a parent"

-- Or Nodes
mkOrNode :: M.Map String Event -> Maybe Id -> Element -> MapWriter Node
mkOrNode eMap mpid e = do
  childList <- mapM (mkSub eMap (Just $ nodeId nid) edgeList) subVs
  let n = OrNode nid
                 mpid
                 (S.fromList . map getNodeId $ childList)
                 (initialId childList)
  insertNPair n
  return n
  where
    nid :: NId
    nid = mkNId e

    subVs :: [Element]
    subVs = getSubV e

    edgeList :: [Edge]
    edgeList = map (mkEdge eMap) $ getEdges e

    initialId :: [Node] -> Id
    initialId childList =
      case getInitial childList of
        Nothing -> error $ "Illegal: Or node " ++ nodeName nid
                        ++ " has no children"
        Just n -> getNodeId n

-- Basic insertion actions

-- Writer computation inserting an Id Node pair into the updates Id to Node map
insertNPair :: Node -> MapWriter ()
insertNPair n = tell $ M.singleton (getNodeId n) n

-- Functions for creating map from XML trigger objects to Events

-- Create a map from event id strings to events from a list of event declaration elements
-- Creates dummy nodes for completion events, to be replaced by the proper nodes by
-- completeSC after all of the nodes in the StateChart have been built
makeEMap :: [Element] -> M.Map String Event
makeEMap = M.fromList . mapMaybe makeEvent
  where
    makeEvent :: Element -> Maybe (String,Event)
    makeEvent e =
      case (findAttr "id" e, findAttr "name" e) of
        (Just idString, Just nameString) ->
          Just (idString, parseEventName nameString)
        _ -> Nothing

    parseEventName :: String -> Event
    parseEventName s =
      if isComplete s
        then Complete $ BasicNode (NId (Id "") s) (Id "") S.empty
        else Event $ Id s

-- Test for if event string specifies a completion event
isComplete :: String -> Bool
isComplete s = "complete(" `isPrefixOf` s && ")" `isSuffixOf` s

-- Writer type for completing StateChart with completions, timers, Vars

-- Type synonym for writer to Id Node Map and Var Set
type SCWriter = Writer (M.Map Id Node)

-- Adds node to the Id map before returning it
returnNode :: Node -> SCWriter Node
returnNode n = tell (M.singleton (getNodeId n) n) >> return n

-- Functions for processing Events and Timer expressions after initial StateChart generation,
-- and enumerating Vars

-- 'complete' the statechart by replacing all dummy nodes in completion events
-- by the nodes specified by name in the event string
completeSC :: StateChart -> StateChart
completeSC sc = sc {root = root', nIds = nIds'}
  where
    (root',nIds') =  runWriter $ completeNode (makeNameMap $ nodes sc)
                                              (nIds sc)
                                              (root sc)

-- Generates a map from names of nodes to their respective nodes
-- only includes nodes whose names are unique
makeNameMap :: S.Set Node -> M.Map String Node
makeNameMap = M.fromList . filterUniqueNames . map (\n -> (getNodeName n, n)) . S.toList where
  eq                  = (==) `on` uncurry const
  filterUniqueNames l = deleteFirstsBy eq (nubBy eq l) $ l \\ nubBy eq l

-- Recurses through the hierarchy tree of the nodes, replacing the nodes of
-- completion events with the node named in the event string
completeNode :: M.Map String Node -> M.Map Id Node -> Node -> SCWriter Node

completeNode nameMap _ (BasicNode nid p es) = do
  let es' = map (completeEdge nameMap) $ S.toList es
  returnNode $ BasicNode nid p (S.fromList es')

completeNode nameMap idMap (AndNode nid p cIds es) = do
  childNodes <- mapM (liftM getNodeId . completeNode nameMap idMap) . S.toList $ toNodes idMap cIds
  let es' = map (completeEdge nameMap) $ S.toList es
  returnNode $ AndNode nid p (S.fromList childNodes) (S.fromList es')

completeNode nameMap idMap (OrNode nid mp cIds d) = do
  childNodes <- mapM (liftM getNodeId . completeNode nameMap idMap) . S.toList $ toNodes idMap cIds
  returnNode $ OrNode nid mp (S.fromList childNodes) d

-- Modifies completion events and timer expressions in an edge to contain
-- the specified node, and enumerates Vars
completeEdge :: M.Map String Node -> Edge -> Edge
completeEdge nameMap (Edge eid s t ev g a) = runReader completeEdge' nameMap where
  completeEdge' = do
    ev' <- completeEvent  ev
    g'  <- completeGuard  g
    a'  <- completeAction a
    return $ Edge eid s t ev' g' a'

-- Type synonym for nameMap reader transformer
type NameReader = Reader (M.Map String Node)

-- Reader lookup function for names in map
lookupName :: String -> NameReader Node
lookupName s = do
  nameMap <- ask
  return $ maybe (error $ "Name " ++ s ++ " not unique node") id (M.lookup s nameMap)

-- Actions for traversing and modifying Events, Guards, Actions, and Expressions
-- for extracting Vars and modifying completion events and timer expressions
-- to contain the specified node

-- Extracts the name of the completion node from the event string
getCompleteName :: String -> String
getCompleteName = init . fromJust . stripPrefix "complete("

-- Modfies completion events to contain the specified node, otherwise identity
completeEvent :: Event -> NameReader Event
completeEvent (Complete n) = liftM Complete . lookupName . getCompleteName $ getNodeName n
completeEvent ev           = return ev

-- Insert all Vars and complete all timer expressions in a Guard statement
completeGuard :: Guard -> NameReader Guard
completeGuard (GNot g)       = liftM GNot $     completeGuard g
completeGuard (BBop b g1 g2) = liftM2 (BBop b) (completeGuard g1)
                                               (completeGuard g2)
completeGuard (IBop b e1 e2) = liftM2 (IBop b) (completeExpr  e1)
                                               (completeExpr  e2)
completeGuard (GIn n)        = liftM GIn . lookupName $ getNodeName n
completeGuard g              = return g

-- Insert all Vars and complete all timer expressions in an Action statement
completeAction :: Action -> NameReader Action
completeAction (Assign v e) = liftM (Assign v) (completeExpr e)
completeAction (Seq a1 a2)  = liftM2 Seq (completeAction a1)
                                         (completeAction a2)
completeAction a            = return a

-- Insert all Vars and complete all timer expressions in an Expr
completeExpr :: Expr -> NameReader Expr
completeExpr (ENeg e)       = liftM  ENeg     (completeExpr  e)
completeExpr (EBop b e1 e2) = liftM2 (EBop b) (completeExpr  e1)
                                              (completeExpr  e2)
completeExpr (EIf g e1 e2)  = liftM3 EIf      (completeGuard g)
                                              (completeExpr  e1)
                                              (completeExpr  e2)
completeExpr (Tmr n)        = liftM Tmr . lookupName $ getNodeName n
completeExpr e              = return e

-- Top-level function from xml element

-- The top-level function from an XML document to a StateChart
toSC :: Element -> Either String StateChart
toSC e = case filterElements isRegion e of
           [e'] ->
             Right $ completeSC $ mkSC eMap e'
             where
               eMap = makeEMap . filterElements f $ e
               f    = maybe False (== "uml:Trigger") . findAttr "type"
           []   -> Left "After parsing XML, no regions found."
           _:_  -> Left "After parsing XML, too many regions."
