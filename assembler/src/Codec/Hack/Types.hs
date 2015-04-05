module Codec.Hack.Types where

import Codec.Hack.Util (pad)

data Binary = BinaryZ | BinaryO deriving (Show, Read, Eq)

data Dest = DestNull
          | DestM
          | DestD
          | DestMD
          | DestA
          | DestAM
          | DestAD
          | DestAMD
            deriving (Show, Read, Eq)

data Jump = JumpNull 
          | JGT
          | JEQ
          | JGE
          | JLT
          | JNE
          | JLE
          | JMP
            deriving (Show, Read, Eq)

data Comp = Comp0
          | Comp1
          | Comp_1
          | CompD
          | CompA
          | CompNotD
          | CompNotA
          | Comp_D
          | Comp_A
          | CompDInc
          | CompAInc
          | CompDDec
          | CompADec
          | CompDPlusA
          | CompDMinusA
          | CompAMinusD
          | CompDAndA
          | CompDOrA
          | CompM
          | CompNotM
          | CompMinusM
          | CompMInc
          | CompMDec
          | CompDPlusM
          | CompDMinusM
          | CompMMinusD
          | CompDAndM
          | CompDOrM
            deriving (Show, Read, Eq)

type Address = [Binary]
                     
data HackCommand = HackA Address | HackC Comp Dest Jump deriving (Show)

data HackCommands = HackCommands [HackCommand]

class AssemblyLanguage a where
    mechanize :: a -> String

instance AssemblyLanguage Binary where
    mechanize BinaryZ = "0"
    mechanize BinaryO = "1"

instance AssemblyLanguage Dest where
    mechanize DestNull = "000"
    mechanize DestM    = "001"
    mechanize DestD     = "010"
    mechanize DestMD    = "011"
    mechanize DestA     = "100"
    mechanize DestAM    = "101"
    mechanize DestAD    = "110"
    mechanize DestAMD   = "111"

instance AssemblyLanguage Jump where
    mechanize JumpNull = "000"
    mechanize JGT      = "001"
    mechanize JEQ      = "010"
    mechanize JGE      = "011"
    mechanize JLT      = "100"
    mechanize JNE      = "101"
    mechanize JLE      = "110"
    mechanize JMP      = "111"

instance AssemblyLanguage Comp where
    mechanize Comp0       = "0101010"
    mechanize Comp1       = "0111111"
    mechanize Comp_1      = "0111010"
    mechanize CompD       = "0001100"
    mechanize CompA       = "0110000"
    mechanize CompNotD    = "0001101"
    mechanize CompNotA    = "0110001"
    mechanize Comp_D      = "0001111"
    mechanize Comp_A      = "0110011"
    mechanize CompDInc    = "0011111"
    mechanize CompAInc    = "0110111"
    mechanize CompDDec    = "0001110"
    mechanize CompADec    = "0110010"
    mechanize CompDPlusA  = "0000010"
    mechanize CompDMinusA = "0010011"
    mechanize CompAMinusD = "0000111"
    mechanize CompDAndA   = "0000000"
    mechanize CompDOrA    = "0010101"
    mechanize CompM       = "1110000"
    mechanize CompNotM    = "1110001"
    mechanize CompMinusM  = "1110011"
    mechanize CompMInc    = "1110111"
    mechanize CompMDec    = "1110010"
    mechanize CompDPlusM  = "1000010"
    mechanize CompDMinusM = "1010011"
    mechanize CompMMinusD = "1000111"
    mechanize CompDAndM   = "1000000"
    mechanize CompDOrM    = "1010101"

instance AssemblyLanguage HackCommand where
  mechanize (HackA address) = pad 16 '0' . concatMap mechanize $ address
  mechanize (HackC comp  dest jump) =
    "111" ++ mechanize comp ++ mechanize dest ++ mechanize jump

instance AssemblyLanguage HackCommands where
    mechanize (HackCommands xs) = unlines (mechanize' xs)
        where
          mechanize' [] = []
          mechanize' (HackA address: as) = 
              (concatMap mechanize (BinaryZ:address)): mechanize' as
          mechanize' (HackC comp dest jump: as) =
              ("111" ++ mechanize comp ++ mechanize dest ++ mechanize jump): mechanize' as

