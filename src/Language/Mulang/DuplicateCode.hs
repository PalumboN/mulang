module Language.Mulang.DuplicateCode (hasDuplicateCode) where


import Language.Mulang
import Language.Mulang.Explorer (expressionsOf)
import qualified Data.Hashable as H (hash)
import Data.List (nub, subsequences, tails, inits)
import Data.List.Split (chunksOf)




hasDuplicateCode :: Expression -> Bool
hasDuplicateCode e =  hasDuplicates $ map hash $ filter (not . isLightweight) (concat $ stripesOf e)


isLightweight :: Expression -> Bool
isLightweight (MuNumber e)              = True
isLightweight (MuString e)              = True
isLightweight (MuBool e)                = True
isLightweight (Variable i)              = True
isLightweight MuNull                    = True
isLightweight Equal                     = True
isLightweight (Application i es)        = not $ any isApplication es
isLightweight (Return e)                = isLightweight e
isLightweight (VariableAssignment i e)  = isLightweight e
isLightweight (VariableDeclaration i e) = isLightweight e
isLightweight _                         = False

isApplication (Application i es) = True
isApplication _                  = False


hasDuplicates ::Eq a => [a] -> Bool
hasDuplicates xs = nub xs /= xs


hash :: Expression -> Int
hash (Return e)                    = 1 * (37 + hash e)
hash (MuNumber e)                  = 2 * H.hash e
hash (MuString e)                  = 3 * H.hash e
hash (Variable i)                  = 5 * H.hash i
hash (MuBool e)                    = 7 * H.hash e
hash (Application i es)            = 11 * (37 + hash i) * (positionalHash es)
hash f@(FunctionDeclaration _ _)   = 13 * (37 + hash (simpleFunctionBody f))
hash f@(ProcedureDeclaration _ _)  = 17 * (37 + hash (simpleProcedureBody f))
hash (Sequence es)                 = 19 * (37 + positionalHash es)
hash _                             = 1

simpleProcedureBody :: Expression -> Expression
simpleProcedureBody (ProcedureDeclaration _ [equation]) = equationUnguardedBody equation  

simpleFunctionBody :: Expression -> Expression
simpleFunctionBody (FunctionDeclaration _ [equation]) = equationUnguardedBody equation  

equationUnguardedBody (Equation _ (UnguardedBody body)) = body


positionalHash :: [Expression] -> Int
positionalHash = sum . zipWith (\index expression -> (31^index) * hash expression) [1..] . reverse 


stripesOf :: Expression -> [[Expression]]
stripesOf = map makeStripes . expressionsOf

makeStripes :: Expression -> [Expression]
--makeStripes (EntryPoint e)               = makeStripes e
--makeStripes n p@(ProcedureDeclaration _ _) = makeStripes n (simpleProcedureBody p)
--makeStripes n f@(FunctionDeclaration _ _)  = makeStripes n (simpleFunctionBody f)
makeStripes (Sequence xs)                = map Sequence . stripes $ xs
makeStripes e                            = [e]


stripes :: [Expression] -> [[Expression]]
--stripes n = filter ( (>n) . length) . subsequences . map Sequence . chunksOf 2 
stripes = filter ( (>1) . length)  . segments . take 16

segments = concatMap tails . inits

lochbaum :: [a] -> [[a]]
lochbaum (x:xs) = [x] :  chunksOf 2 xs  ++ chunksOf 2 (x:xs)
