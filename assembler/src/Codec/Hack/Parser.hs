module Codec.Hack.Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import Codec.Hack.Types

parseDest :: Parser Dest
parseDest = do
  s <- try ((string "null") <|>
            many1 (oneOf "ADM"))
  char '='
  return $ case s of
    "null" -> DestNull
    "M"    -> DestM
    "D"    -> DestD
    "MD"   -> DestMD
    "A"    -> DestA
    "AM"   -> DestAM
    "AD"   -> DestAD
    "AMD"  -> DestAMD

parseJump :: Parser Jump
parseJump = do
  char ';'
  s <- (try (string "null") <|>
        try (string "JGT")  <|>
        try (string "JEQ")  <|>
        try (string "JGE")  <|>
        try (string "JLT")  <|>
        try (string "JNE")  <|>
        try (string "JLE")  <|>
        try (string "JMP") )
  return $ case s of
    "null" -> JumpNull
    _      -> read s

parseComp :: Parser Comp
parseComp = do
  s <- (string "0"         <|>
        string "1"         <|>
        try (string "-1")  <|>
        try (string "!D")  <|>
        try (string "!A")  <|>
        try (string "-D")  <|>
        try (string "-A")  <|>
        try (string "D+1") <|>
        try (string "A+1") <|>
        try (string "D-1") <|>
        try (string "A-1") <|>
        try (string "D+A") <|>
        try (string "D-A") <|>
        try (string "A-D") <|>
        try (string "D&A") <|>
        try (string "D|A") <|>
        try (string "!M")  <|>
        try (string "-M")  <|>
        try (string "M+1") <|>
        try (string "M-1") <|>
        try (string "D+M") <|>
        try (string "D-M") <|>
        try (string "M-D") <|>
        try (string "D&M") <|>
        try (string "D|M") <|>
        try (string "M")   <|>
        try (string "A")   <|>
        try (string "D"))
  return $ case s of
    "0" -> Comp0
    "1" -> Comp1
    "-1" -> Comp_1
    "D" -> CompD
    "A" -> CompA
    "!D" -> CompNotD
    "!A" -> CompNotA
    "-D" -> Comp_D
    "-A" -> Comp_A
    "D+1" -> CompDInc
    "A+1" -> CompAInc
    "D-1" -> CompDDec
    "A-1" -> CompADec
    "D+A" -> CompDPlusA
    "D-A" -> CompDMinusA
    "A-D" -> CompAMinusD
    "D&A" -> CompDAndA
    "D|A" -> CompDOrA
    "M" -> CompM
    "!M" -> CompNotM
    "-M" -> CompMinusM
    "M+1" -> CompMInc
    "M-1" -> CompMDec
    "D+M" -> CompDPlusM
    "D-M" -> CompDMinusM
    "M-D" -> CompMMinusD
    "D&M" -> CompDAndM
    "D|M" -> CompDOrM

symbol :: Parser String
symbol = do
  s <- letter <|> oneOf "$_.:"
  ss <- many (letter <|> oneOf "$_.:" <|> digit)
  return $ s:ss
  

parseHackCommand :: Parser HackAssembly
parseHackCommand = do
  mdest <- optionMaybe . try $ parseDest
  comp <- parseComp
  mjump <- optionMaybe parseJump
  let dest = maybe DestNull id mdest
      jump = maybe JumpNull id mjump
  return $ AssemblyC $ HackC comp dest jump

parseHackAddress :: Parser HackAssembly
parseHackAddress = do
  char '@'
  s <- many1 digit
  return . AssemblyA . HackA. dec2bin $ (read s)

parseHackVariable :: Parser HackAssembly
parseHackVariable = do
  char '@'
  s <- symbol
  return . AssemblyV $ s

parseHackLabel :: Parser HackAssembly
parseHackLabel = do
  char '('
  s <- symbol
  char ')'
  return . AssemblyL $ s

parseHackCommands :: Parser HackAssembly
parseHackCommands = try parseHackAddress
                    <|> parseHackVariable
                    <|> parseHackCommand
                    <|> parseHackLabel

parseComment :: Parser ()
parseComment = do
  optionMaybe (string "//" >> (many $ noneOf "\n"))
  many1 space
  return ()

parseEmptyLine :: Parser ()
parseEmptyLine = many space >> return ()

parseLine :: Parser HackAssembly
parseLine = do
  spaces
  c <- parseHackCommands
  (many parseComment >> return ()) <|> spaces
  return c

parseHackAsm :: Parser [HackAssembly]
parseHackAsm = do
  many parseComment
  many parseLine


