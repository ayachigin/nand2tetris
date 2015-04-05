module Codec.Hack.Util (pad) where

pad :: Int -> Char -> String -> String
pad n c s
  | length s < n = ps ++ s
  where
    ls = length s
    cs = repeat c
    sn = n - ls
    ps = take sn cs
