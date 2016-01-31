module Language.Mulang.Explorer (
  (//),
  expressionsOf,
  equationBodiesOf,
  referencesOf,
  declarationsOf,
  referencedBindingsOf,
  declaredBindingsOf,
  bindedDeclarationsOf,
  transitiveReferencedBindingsOf,
  nameOf,
  extractDeclaration,
  extractReference,
  Expression(..),
  Binding) where

import Language.Mulang
import Data.Maybe (maybeToList)
import Data.List (nub)

type Binding = String

(//)  :: Expression -> Binding -> [Expression]
(//) = flip bindedDeclarationsOf

expressionsOf :: Expression -> [Expression]
expressionsOf expr = expr : concatMap expressionsOf (subExpressions expr)
  where
    subExpressions :: Expression -> [Expression]
    subExpressions (VariableDeclaration _ v)          = [v]
    subExpressions (FunctionDeclaration _ equations)  = expressionsOfEquations equations
    subExpressions (ProcedureDeclaration _ equations) = expressionsOfEquations equations
    subExpressions (MethodDeclaration _ equations)    = expressionsOfEquations equations
    subExpressions (AttributeDeclaration _ v)         = [v]
    subExpressions (ObjectDeclaration _ v)            = [v]
    subExpressions (Application a bs)                 = a:bs
    subExpressions (Send e1 e2 e3)                    = [e1, e2] ++ e3
    subExpressions (Lambda _ a)                       = [a]
    subExpressions (If a b c)                         = [a, b, c]
    subExpressions (While e1 e2)                      = [e1, e2]
    subExpressions (Match e1 equations)               = e1:expressionsOfEquations equations
    subExpressions (Comprehension a _)                = [a] --TODO
    subExpressions (Return v)                         = [v]
    subExpressions (Sequence es)                      = es
    subExpressions (MuObject es)                      = [es]
    subExpressions (MuTuple as)                       = as
    subExpressions (MuList as)                        = as
    subExpressions _                                  = []

    expressionsOfEquations eqs = eqs >>= \(Equation _ body) -> topExpressionOfBody body
    topExpressionOfBody (UnguardedBody e)      = [e]
    topExpressionOfBody (GuardedBody b)        = b >>= \(es1, es2) -> [es1, es2]

equationBodiesOf :: Expression -> [EquationBody]
equationBodiesOf = concatMap bodiesOf . expressionsOf
  where
    bodiesOf :: Expression -> [EquationBody]
    bodiesOf (FunctionDeclaration _ equations) = map (\(Equation _ body) -> body) equations
    bodiesOf _ = []

referencesOf :: Expression -> [(Binding, Expression)]
referencesOf = nub . concatMap (maybeToList . extractReference) . expressionsOf

declarationsOf :: Expression -> [(Binding, Expression)]
declarationsOf = concatMap (maybeToList . extractDeclaration) . expressionsOf

referencedBindingsOf :: Expression -> [Binding]
referencedBindingsOf = map fst . referencesOf

declaredBindingsOf :: Expression -> [Binding]
declaredBindingsOf = map fst . declarationsOf

bindedDeclarationsOf :: Binding -> Expression -> [Expression]
bindedDeclarationsOf binding = map snd . filter ((==binding).fst) . declarationsOf

transitiveReferencedBindingsOf :: Binding -> Expression -> [Binding]
transitiveReferencedBindingsOf binding code =  expand (concatMap referencedBindingsOf . (`bindedDeclarationsOf` code)) binding
  where
    expand :: Eq a => (a-> [a]) -> a -> [a]
    expand f x = expand' [] f [x]

    expand' _ _ [] = []
    expand' ps f (x:xs) | elem x ps = expand' ps f xs
                        | otherwise = [x] ++ expand' (x:ps) f (xs ++ f x)


nameOf :: Expression -> Maybe Binding
nameOf = fmap fst . extractDeclaration

extractReference :: Expression -> Maybe (Binding, Expression)
extractReference e@(Variable    q) = Just (q, e)
extractReference _                 = Nothing

extractDeclaration :: Expression -> Maybe (Binding, Expression)
extractDeclaration e@(TypeSignature n)         = Just (n, e)
extractDeclaration e@(TypeAliasDeclaration n ) = Just (n, e)
extractDeclaration e@(VariableDeclaration n _) = Just (n, e)
extractDeclaration e@(FunctionDeclaration n _) = Just (n, e)
extractDeclaration e@(RecordDeclaration n)     = Just (n, e)
extractDeclaration e@(ProcedureDeclaration n _)= Just (n, e)
extractDeclaration e@(ObjectDeclaration n _)   = Just (n, e)
extractDeclaration e@(MethodDeclaration n _)   = Just (n, e)
extractDeclaration e@(AttributeDeclaration n _)= Just (n, e)
extractDeclaration _                           = Nothing




