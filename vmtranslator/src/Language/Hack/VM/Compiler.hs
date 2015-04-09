module Language.Hack.VM.Compiler (compile) where

import Control.Monad.RWS (RWS, runRWS, get, put, modify, tell, ask)
import Prelude hiding (compare)

import Codec.Hack.Types (Jump(..))
import Language.Hack.VM.Types
import Language.Hack.VM.Parser (parseHackVM)
    
type CodeWriter a = RWS String () Int a
    
getFilename :: CodeWriter String
getFilename = ask

getVariableCout :: CodeWriter Int
getVariableCout = do
  c <- get
  modify (+1)
  return c
               
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

stack :: StackOperation -> CodeWriter String
stack (Push Constant n) = return $ pushA n
stack (Push Pointer n)  = return $ pushM $ show (baseAddress Pointer + n)
stack (Push Temp n)     = return $ pushM $ show (baseAddress Temp + n)
stack (Push Static n)   = getFilename >>= return . pushM . (++ "." ++ show n)
stack (Push segment n)  = return $ unlines ["@" ++ show n
                                           ,"D=A"
                                           ,"@" ++ segmentToLabel segment
                                           ,"A=D+M"
                                           ,"D=M"
                                           ,init pushD]
-- stack (Pop Temp n)      = return $ popS $ show (baseAddress Temp + n)

arithmetic :: ArithmeticCommand -> CodeWriter String
arithmetic Add = return $ "// Add\n" ++ popD ++ popM ++ add ++ pushD
arithmetic Sub = return $ "// Sub\n" ++ popD ++ popM ++ sub ++ pushD
arithmetic Neg = return $ "// Neg\n" ++ popD ++ neg ++ pushD
arithmetic Eq  = compare JEQ
arithmetic Gt  = compare JGT
arithmetic Lt  = compare JLT
arithmetic And = return $ popD ++ popM ++ "D=D&M\n" ++ pushD
arithmetic Or  = return $ popD ++ popM ++ "D=D|M\n" ++ pushD
arithmetic Not = return $ popM ++ "D=!M\n" ++ pushD

segmentToLabel :: MemorySegment -> String
segmentToLabel Argument = "ARG"
segmentToLabel Local    = "LCL"
segmentToLabel This     = "THIS"
segmentToLabel That     = "THAT"
segmentToLabel segment  = error $ "Unexpected segment to label: " ++ show segment

baseAddress :: MemorySegment -> Int
baseAddress Pointer  = 3
baseAddress Temp     = 5
baseAddress segment  = error $ "Unexpected segment to address: " ++ show segment

pushA :: Int -> String
pushA n = unlines ["@" ++ show n
                  ,"D=A"
                  ,init pushD]
                   
pushM :: String -> String
pushM var = unlines ["@" ++ var
                    ,"D=M"
                    ,init pushD]

pushD :: String
pushD = unlines ["@SP // Push value in D register to *SP"
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

compare :: Jump -> CodeWriter String
compare jmp = do
  substruction <- arithmetic Sub
  not' <- arithmetic Not
  filename <- getFilename
  v <- getVariableCout
  let label1 = filename ++ "$compare_true" ++ show v
      label2 = filename ++ "$push_compare_result" ++ show v
  return $ unlines ["// compare"
                   ,init substruction
                   ,"@" ++ label1
                   ,"D;" ++ show jmp
                   ,init popM
                   ,"D=0"
                   ,init pushD
                   ,"@" ++ label2
                   ,"0;JMP"
                   ,"(" ++ label1 ++ ")"
                   ,init popM
                   ,"D=0"
                   ,init pushD
                   ,init not'
                   ,"(" ++ label2 ++ ")"
                   ]

