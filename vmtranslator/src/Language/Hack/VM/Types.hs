module Language.Hack.VM.Types where

data ArithmeticCommand = Add 
                       | Sub
                       | Neg
                       | Eq
                       | Gt
                       | Lt
                       | And
                       | Or
                       | Not
                         deriving (Show, Read, Eq)

data MemorySegment = Argument
                   | Local
                   | Static
                   | Constant
                   | This
                   | That
                   | Pointer
                   | Temp
                     deriving (Show, Read, Eq)

data StackOperation = Push MemorySegment Int
                    | Pop  MemorySegment Int
                      deriving (Show, Read, Eq)

data HackVMCommand = ArithmeticCommand ArithmeticCommand
                   | StackOperation StackOperation
                     deriving (Show, Read, Eq)

data HackVML = HackVML [HackVMCommand]
             deriving (Show, Read, Eq)
