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
push = unlines ["@SP // Push value in D register to *SP"
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

eq :: CodeWriter String
eq = do
  substruction <- arithmetic Sub
  not' <- arithmetic Not
  filename <- getFilename
  v <- getVariableCout
  let label1 = filename ++ "$not_eq" ++ show v
      label2 = filename ++ "$push_result" ++ show v
  return $ unlines ["// eq"
                   ,init substruction
                   ,"@" ++ label1
                   ,"D;JNE"
                   -- D register is 0 and not 0 is 0xFFFF <-- its true !
                   ,init not' -- remove \n
                   ,"@" ++ label2
                   ,"0;JMP"
                   ,"(" ++ label1 ++ ")"
                   ,init popD
                   ,"D=0"
                   ,init push -- remove \n
                   ,"(" ++ label2 ++ ")"
                   ]
                   
type CodeWriter a = RWS String () Int a
    
compile :: HackVML -> String -> String
compile (HackVML vml) filename = case runRWS (compile' vml) filename 0 of
                                   (s, _, _) -> s
    where
      compile' :: [HackVMCommand] -> CodeWriter String
      compile' [] = return ""
      compile' (StackOperation c:cs)    = go stack c cs
      compile' (ArithmeticCommand c:cs) = go arithmetic c cs
      go f c cs = do
        e1 <- f c
        e2 <- compile' cs
        return $ e1 ++ e2

getFilename :: CodeWriter String
getFilename = ask

getVariableCout :: CodeWriter Int
getVariableCout = do
  c <- get
  modify (+1)
  return c
               
stack :: StackOperation -> CodeWriter String
stack (Push Constant n) = return $ pushA n
stack (Push seg n)      = return $ pushM $ show (baseAddress seg + n)
-- stack (Pop Temp n)      = return $ popS $ show (baseAddress Temp + n)

arithmetic :: ArithmeticCommand -> CodeWriter String
arithmetic Add = return $ "// Add\n" ++ popD ++ popM ++ add ++ push
arithmetic Sub = return $ "// Sub\n" ++ popD ++ popM ++ sub ++ push
arithmetic Neg = return $ "// Neg\n" ++ popD ++ neg ++ push
arithmetic Eq  = eq
arithmetic Gt  = return $ undefined
arithmetic Lt  = return $ undefined
arithmetic And = return $ popD ++ popM ++ "D=D&M\n" ++ push
arithmetic Or  = return $ popD ++ popM ++ "D=D|M\n" ++ push
arithmetic Not = return $ popM ++ "D=!M\n" ++ push
