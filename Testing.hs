module Testing where

import TyGen 

s = [C, A, A, A, G, A, G, A, A, T, C, C, T, C, T, T, T, G, A, T]
enz = [PyR, COn, PuR, Cut]
cont = initContext 2 enz s
ppC = putStrLn . showContext
