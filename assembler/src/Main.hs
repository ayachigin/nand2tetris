module Main where

import Data.Map ((!), member, insert)
import Text.Parsec (parse)
import System.Environment (getArgs)

import Codec.Hack.Parser (parseHackAsm)
import Codec.Hack.Symbol (SymbolTable, mkSymbolTable)
import Codec.Hack.Types
import Codec.Hack.Util (pad)

data SymbolAssembly = SymbolAssembly SymbolTable [HackAssembly]

main :: IO ()
main = do
  [file] <- getArgs
  asm    <- readFile file
  let hack = parse parseHackAsm "hack" asm
      (fname, _) = break (=='.') file
      out = fname ++ ".hack"
  case hack of
      (Right a) -> do
        let symbolTable = mkSymbolTable a
        writeFile out $ mechanize $ SymbolAssembly symbolTable a
      (Left b)  -> error $ show b

instance AssemblyLanguage SymbolAssembly where
  mechanize (SymbolAssembly table xs) = unlines $ mechanize' 16 table xs
    where
      mechanize' _ _ [] = []
      mechanize' n t (AssemblyA (HackA a): as) =
        (pad 16 '0' . concatMap mechanize) a: mechanize' n t as
      mechanize' n t (AssemblyC (HackC c d j): as) = 
        ("111" ++ mechanize c ++ mechanize d ++ mechanize j): mechanize' n t as
      mechanize' n t (AssemblyL _: as) = mechanize' n t as
      mechanize' n t (AssemblyV s: as) = 
        if s `member` t then
          mechanize' n t $ variable2AssemblyA s t : as
        else
          let newTable = insert s n t in
          mechanize' (n+1) newTable $ variable2AssemblyA s newTable: as
      variable2AssemblyA s t = AssemblyA $ HackA $ dec2bin $ t!s
