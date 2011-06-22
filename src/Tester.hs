module Main where

import CCO.Component             hiding (parser)
import CCO.Feedback

import CCO.Tree                  (ATerm, Tree (toTree, fromTree), parser)
import CCO.Printing              (render_, Doc)
import Control.Arrow             (Arrow (arr), (>>>))
import Control.Monad             
import MF.Languages.PHP.AG       (Node, solve, simplifier, checker, reporter, annotator)

import Control.Exception
import System.Directory
import System.FilePath
import System.IO
import System.Process

reader :: Component ATerm Node
reader = component toTree

render :: Component Doc String
render = component $ return . render_ 80

pipeline :: Component String String
pipeline = parser >>> reader >>> annotator >>> simplifier >>> checker >>> reporter >>> render 


isPhpFile name = takeExtension name == ".php"

-- runSuite :: (a -> Feedback b) -> IO ()
runSuite pipeline = do currentDirectory  <- getCurrentDirectory
                       let testDirectory = currentDirectory </> "tests"
                       let tmpFile       = testDirectory </> "tmp"
                       names             <- getDirectoryContents testDirectory
                       let files         = filter isPhpFile names
                       results           <- forM files $ \file -> do 
                           pid     <- runCommand $ "cat " ++ (testDirectory </> file) ++ " | ./parse > " ++ tmpFile
                           waitForProcess pid
                           content <- readFile tmpFile
                           removeFile tmpFile
                           result  <- runFeedback (runComponent pipeline content) 1 1 stderr 
                           case result of
                               Nothing     -> error $ "Something went wrong with: " ++ file
                               Just output -> return output
                       print results
                       
              
main = runSuite pipeline 
          --      contents <- sequence $ map readFile files 
--              return files 
--              print (length contents)
              
              {-
          result <- runFeedback (f input) 1 1 stderr
          case result of
              Nothing     -> exitFailure
              Just output -> putStrLn output >> exitWith ExitSuccess
          -}