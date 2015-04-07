module Language.Hack.VM.Parser where

import Text.Parsec
import Text.Parsec.String
import Control.Applicative ((<$>), (<*>), (<*), (*>))

import Language.Hack.VM.Types

    
add :: Parser ArithmeticCommand
add = string "add" *> return Add

sub :: Parser ArithmeticCommand
sub = string "sub" >> return Sub
      
neg :: Parser ArithmeticCommand
neg = string "neg" >> return Neg

eq :: Parser ArithmeticCommand
eq = string "eq" >> return Eq

gt :: Parser ArithmeticCommand
gt = string "gt" >> return Gt

lt :: Parser ArithmeticCommand
lt = string "lt" >> return Lt

and :: Parser ArithmeticCommand
and = string "and" >> return And

or :: Parser ArithmeticCommand
or = string "or" >> return Or

not :: Parser ArithmeticCommand
not = string "not" >> return Not

arithmeticCommand :: Parser ArithmeticCommand
arithmeticCommand = try add <|>
                    sub <|>
                    try neg <|>
                    eq <|>
                    gt <|>
                    lt <|>
                    Language.Hack.VM.Parser.and <|>
                    Language.Hack.VM.Parser.or <|>
                    Language.Hack.VM.Parser.not

argument :: Parser MemorySegment
argument = string "argument" >> return Argument

local :: Parser MemorySegment
local = string "local" >> return Local

static :: Parser MemorySegment
static = string "static" >> return Static
        
constant :: Parser MemorySegment
constant = string "constant" >> return Constant

this :: Parser MemorySegment
this = string "this" >> return This

that :: Parser MemorySegment
that = string "that" >> return That

pointer:: Parser MemorySegment
pointer= string "pointer" >> return Pointer

temp:: Parser MemorySegment
temp= string "temp" >> return Temp

memorySegment :: Parser MemorySegment
memorySegment = argument <|>
                local <|>
                static <|>
                constant <|>
                try this <|>
                try that <|>
                pointer <|>
                temp

push :: Parser StackOperation
push = do
  string "push"
  spaces
  seg <- memorySegment
  spaces
  d <- many1 digit
  return $ Push seg (read d)

pop :: Parser StackOperation
pop = do
  string "pop"
  spaces
  seg <- memorySegment
  spaces
  d <- many1 digit
  return $ Pop seg (read d)

stackOperation :: Parser StackOperation
stackOperation = try push <|> pop

parserHackVM :: Parser [HackVMCommand]
parserHackVM = do 
  spaces
  sepEndBy (a <|> s) spaces
      where
        a = do
          ac <- try arithmeticCommand
          return $ ArithmeticCommand ac
        s = do
          so <- stackOperation
          return $ StackOperation so

parseHackVM :: String -> String -> Either ParseError [HackVMCommand]
parseHackVM = parse parserHackVM
