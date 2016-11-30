module Main where

import Language.CSPM.SpecGen.XML2UML
import Language.CSPM.SpecGen.UML2CSP
import Language.CSPM.SpecGen.UMLSyntax

import Language.CSPM.Pretty

import System.Environment
import System.Console.GetOpt

import Text.XML.Light.Input

import Control.Monad (unless)

stringToStateChart :: String -> Either String StateChart
stringToStateChart input =
  case parseXMLDoc input of
    Nothing  -> Left "Error in parsing XML."
    Just xml -> toSC xml

-- the first string is the CSP, the second is a list of warnings
stateChartToCSPM :: StateChart -> (String,[String])
stateChartToCSPM = emitMod . toCSP

data SGOptions = SGOpt {printInput :: Bool,
                        printHelp :: Bool}

defaultOpts :: SGOptions
defaultOpts = SGOpt {printInput = False,
                     printHelp = False}

optDescr :: [OptDescr (SGOptions -> SGOptions)]
optDescr = [Option "p" ["printinput"] (NoArg (\o -> o {printInput=True}))
                   "Display the parsed input statechart",
            Option "h" ["help"] (NoArg (\o -> o {printHelp=True}))
                   "Diplay this help message"]
           
main :: IO ()
main = do
  args <- getArgs
  let (optFs,files,errs) = getOpt Permute optDescr args
      SGOpt {printInput,printHelp} = foldl (flip ($)) defaultOpts optFs
  case (errs,printHelp,printInput,files) of
    -- the case for cmdline parsing errors
    (_:_,_,_,_) ->
      error $ usage errs
    -- the case where we are asked to print help
    (_,True,_,_) -> error $ usage []
    -- the case where we are only asked to print the input
    (_,_,True,xmlFile:[]) -> do
      xml <- readFile xmlFile
      case stringToStateChart xml of
        Left err -> error err
        Right sc -> putStrLn $ prettyChart sc
    -- the case where we translate the statechart
    (_,_,_,(xmlFile : cspFile : [])) -> do
      xml  <- readFile xmlFile
      case stringToStateChart xml of
        Left err -> error err
        Right sc ->
          let (cspMod,warnings) = stateChartToCSPM sc in
          do unless (null warnings) $ do
               putStrLn $ "The pretty printer yielded the following "
                       ++ show (length warnings) ++ " warning(s)."
               mapM_ putStrLn warnings
             writeFile cspFile cspMod
    -- more than two file names specified
    (_,_,_,_:_:_) -> error $ usage ["Too many arguments"]
    -- not enough file names specified
    (_,_,_,_) -> error $ usage ["Too few arguments"]
  where
    header :: String
    header = "Usage: specgen [OPTIONS] INFILE [OUTFILE]"

    usage :: [String] -> String
    usage errs = concat errs ++ "\n" ++ usageInfo header optDescr
