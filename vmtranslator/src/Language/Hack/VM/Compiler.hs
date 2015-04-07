module Language.Hack.VM.Compiler where

import Control.Monad.RWS (RWS, runRWS, get, put, modify, tell, ask)
    
import Language.Hack.VM.Types
import Language.Hack.VM.Parser (parseHackVM)
    
type RAMAddress = (Int, Int)

stackRAM :: RAMAddress
stackRAM = (256, 2047)

heapRAM :: RAMAddress
heapRAM = (2048, 16383)

memoryMappedIO :: RAMAddress
memoryMappedIO = (16384, 24575)

freeArea :: RAMAddress
freeArea = (24576, 32767)

tempRAM :: RAMAddress
tempRAM = (5, 12)
           
baseAddress :: MemorySegment -> Int
baseAddress Argument = undefined
baseAddress Temp = 5

pushA :: Int -> String
pushA n = unlines ["@" ++ show n
                  ,"D=A"] ++
          push
                   
pushM :: String -> String
pushM var = unlines ["@" ++ var
                    ,"D=M"] ++
            push

push :: String
push = unlines ["@SP"
               ,"A=M"
               ,"M=D"
               ,"@SP"
               ,"M=M+1"]

popM :: String
popM = unlines ["@SP"
               ,"M=M-1"
               ,"A=M"]

popD :: String
popD = popM ++ unlines ["D=M"]
       
popS :: String -> String
popS symbol = popM ++ popD ++
              unlines ["@" ++ symbol
                      ,"M=D"]

-- | Assume M is left hand side operand.
add :: String
add = unlines ["D=D+M"]

sub :: String
sub = unlines ["D=M-D"]

neg :: String
neg = unlines ["D=-M"]
                   
class Reifiable a where
    reify :: a -> String

instance Reifiable HackVMCommand where
    reify (StackOperation (Push Constant n)) = pushA n
    reify (StackOperation (Push seg n)) = pushM $ show (baseAddress seg + n)
    reify (StackOperation (Pop Temp n)) = popS $ show (baseAddress Temp + n)
    reify (ArithmeticCommand cmd) = reifyArithmeticCommand cmd

reifyArithmeticCommand :: ArithmeticCommand -> String
reifyArithmeticCommand Add = popD ++ popM ++ add ++ push
reifyArithmeticCommand Sub = popD ++ popM ++ sub ++ push
reifyArithmeticCommand Neg = popD ++ neg ++ push
reifyArithmeticCommand Eq  = undefined
reifyArithmeticCommand Gt  = undefined
reifyArithmeticCommand Lt  = undefined
reifyArithmeticCommand And = popD ++ popM ++ "D=D&M\n" ++ push
reifyArithmeticCommand Or  = popD ++ popM ++ "D=D|M\n" ++ push
reifyArithmeticCommand Not = popM ++ "D=!M\n" ++ push

