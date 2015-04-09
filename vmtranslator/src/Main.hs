module Main where

import System.Environment (getArgs)
import System.FilePath (takeFileName, dropExtensions, replaceFileName)

import Language.Hack.VM.Compiler (compile)
import Language.Hack.VM.Parser (parseHackVM)

main :: IO ()
main = do
  [file] <- getArgs
  s <- readFile file
  let filename = dropExtensions . takeFileName $ file
      out = replaceFileName file $ filename ++ ".asm"
      vml = parseHackVM "Hack VM" s
  case vml of
    Right v  -> do
             let asm = compile v filename
             writeFile out asm
    Left err -> print err
