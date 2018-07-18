-- Simple environment to format Markdown documents.

import HTML.Base
import System.IO
import Markdown
import System.Environment
import System.Process

main :: IO ()
main = do
  opts <- getArgs >>= processOptions defaultOpts
  let infile  = inFile opts
      outfile = outFile opts
  case target opts of
    PDF  -> if null infile
             then formatMarkdownInputAsPDF
             else formatMarkdownFileAsPDF infile
    TEX  -> do mdtxt <- if null infile then getContents else readFile infile
               let texdoc = if standAlone opts
                              then markdownText2CompleteLaTeX mdtxt
                              else markdownText2LaTeX mdtxt
               if null outfile then putStrLn texdoc
                               else writeFile outfile texdoc
    HTML -> do mdtxt <- if null infile then getContents else readFile infile
               let htmldoc = if standAlone opts
                               then markdownText2CompleteHTML (title opts) mdtxt
                               else showHtmlExps (markdownText2HTML mdtxt)
               if null outfile then putStrLn htmldoc
                               else writeFile outfile htmldoc


data Target = PDF | TEX | HTML

data Options = Options
  { target     :: Target
  , inFile     :: String
  , outFile    :: String
  , title      :: String
  , standAlone :: Bool
  }

defaultOpts :: Options
defaultOpts = Options PDF "" "" "" True

processOptions :: Options -> [String] -> IO Options
processOptions opts args = case args of
  []                 -> return opts
  ("--tex"     : margs)  -> processOptions opts { target = TEX } margs
  ("--html"    : margs)  -> processOptions opts { target = HTML } margs
  ("-o":fname  : margs)  -> processOptions opts { outFile = fname } margs
  ("-i"        : margs)  -> processOptions opts { standAlone = False } margs
  ("--include" : margs)  -> processOptions opts { standAlone = False } margs
  ("-t" : s    : margs)  -> processOptions opts { title = s } margs
  ("--title":s : margs)  -> processOptions opts { title = s } margs
  ['-':_] -> showError $ "Illegal arguments: " ++ unwords args ++ "\n" ++ howTo
  [fname] -> return opts { inFile = fname }
  _       -> showError $ "Illegal arguments: " ++ unwords args ++ "\n" ++ howTo
 where
  showError s = putStrLn s >> exitWith 1

howTo :: String
howTo =
  "md2pdf [-o outfile] [-i|--include] [[-t|--title] title] [--tex|--html] [infile]"
