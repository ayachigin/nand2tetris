module Main where

import System.Environment (getArgs)
import System.FilePath (takeFileName, dropExtensions, replaceFileName)

import Language.Hack.VM.Compiler (reify)
import Language.Hack.VM.Parser (parseHackVM)

main :: IO ()
main = do
  [file] <- getArgs
  s <- readFile file
  let out = replaceFileName file $ (dropExtensions . takeFileName $ file) ++ ".asm"
      (Right vms) = parseHackVM "Hack VM" s
      asm = concatMap reify $ vms
  writeFile out asm
