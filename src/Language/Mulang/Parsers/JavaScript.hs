module Language.Mulang.Parsers.JavaScript (js, parseJavaScript) where

import Language.Mulang
import Language.Mulang.Builder

import Language.JavaScript.Parser.Parser
import Language.JavaScript.Parser.AST

import Data.Maybe (fromJust)

js = fromJust.parseJavaScript

parseJavaScript :: String -> Maybe Expression
parseJavaScript code = Just . mu $ readJs code

mu :: JSNode -> Expression
mu = compact . muNode . gc
  where
    muNode (JSSourceElementsTop statements)                  = [compactMapMu statements]
    muNode (JSIdentifier n)                                  = [Variable n]
    muNode (JSDecimal val)                                   = [MuNumber (read val)]
    muNode (JSExpression [x])                                = [mu x]
    muNode (JSExpression [x, y])
                        | (JSArguments _ args _) <- gc y     = [Application (mu x) (mapMu args)]
    muNode (JSExpression xs)                                 = [compactMapMu xs]
    muNode (JSLiteral "true")                                = [MuBool True]
    muNode (JSLiteral "false")                               = [MuBool False]
    muNode (JSLiteral _)                                     = []
    muNode (JSElision _)                                     = []
    muNode (JSHexInteger v)                                  = muNode (JSStringLiteral '\'' v)
    muNode (JSOctal v)                                       = muNode (JSStringLiteral '"' v)
    muNode (JSStringLiteral _ v)                             = [MuString v]
    muNode (JSVariables _ decls _)                           = mapMu decls
    muNode (JSArrayLiteral _ es _)                           = [MuList (mapMu es)]
    muNode (JSVarDecl var initial)                           = [muVar var initial]
    muNode (JSWith _ _ _ _ _)                                = [ExpressionOther]
    muNode (JSExpressionTernary cond _ true _ false)         = [muIf cond   true false]
    muNode (JSIf _ _ cond _ true false)                      = [muIf [cond] true false]
    muNode (JSWhile _ _ cond _ action)                       = [muWhile cond action]
    muNode (JSBlock _ exps _)                                = [compactMapMu exps]
    muNode (JSFunctionExpression _ [] _ params _ body)       = [Lambda (muParams params) (mu body)]
    muNode (JSFunctionExpression _ [name] _ params _ body)   = [muFunction  name params body]
    muNode (JSReturn _ e _)                                  = [compactMapMu e]
    muNode (JSExpressionParen _ e _)                         = [mu e]
    muNode (JSObjectLiteral _ es _)                          = [MuObject (compactMapMu es)]
    muNode (JSLabelled _ _ e)                                = [mu e]
    muNode (JSPropertyNameandValue var _ initial)            = [muVar var initial]
    muNode (JSFunction _ name _ params _ body)               = [muFunction name params body]
    muNode (JSFunction _ name _ params _ body)               = [muFunction name params body]
    muNode (JSExpressionBinary op l _ r)                     = [InfixApplication (compactMapMu l) op (compactMapMu r)]
    muNode (JSMemberDot receptor _ selector)                 = [Send (compactMapMu receptor) (mu selector) []]
    muNode e = error (show e)

    mapMu = concatMap (muNode . gc)

    muParams :: [JSNode] -> [Pattern]
    muParams params = concatMap (muPattern.gc) params

    muPattern (JSLiteral _)    = []
    muPattern (JSIdentifier i) = [VariablePattern i]
    muPattern e                = error (show e)

    muIdentifier (JSIdentifier id) = id

    gc (NN n) = n
    gc (NT n _ _) = n

    compactMapMu :: [JSNode] -> Expression
    compactMapMu  = compact . mapMu

    muIf :: [JSNode] -> [JSNode] -> [JSNode] -> Expression
    muIf cond true false = If (compactMapMu cond) (compactMapMu true) (compactMapMu false)

    muWhile cond action  = While (mu cond) (mu action)

    muVar (NT (JSIdentifier var) _ _) initial = VariableDeclaration var (compactMapMu initial)

    muFunction :: JSNode -> [JSNode] -> JSNode -> Expression
    muFunction name params body =  FunctionDeclaration ((muIdentifier.gc) name) [Equation (muParams params) (UnguardedBody (mu body))]


{-JSRegEx String
optional lb,optional block statements,optional rb
JSBreak JSNode [JSNode] JSNode
break, optional identifier, autosemi
JSCallExpression String [JSNode] [JSNode] [JSNode]
type : ., (), []; opening [ or ., contents, closing
JSCase JSNode JSNode JSNode [JSNode]
case,expr,colon,stmtlist
JSCatch JSNode JSNode JSNode [JSNode] JSNode JSNode
catch,lb,ident,[if,expr],rb,block
JSContinue JSNode [JSNode] JSNode
continue,optional identifier,autosemi
JSDefault JSNode JSNode [JSNode]
default,colon,stmtlist
JSDoWhile JSNode JSNode JSNode JSNode JSNode JSNode JSNode
do,stmt,while,lb,expr,rb,autosemi
JSExpressionPostfix String [JSNode] JSNode
type, expression, operator
JSExpressionTernary [JSNode] JSNode [JSNode] JSNode [JSNode]
cond, ?, trueval, :, falseval
JSFinally JSNode JSNode
finally,block
JSFor JSNode JSNode [JSNode] JSNode [JSNode] JSNode [JSNode] JSNode JSNode
for,lb,expr,semi,expr,semi,expr,rb.stmt
JSForIn JSNode JSNode [JSNode] JSNode JSNode JSNode JSNode
for,lb,expr,in,expr,rb,stmt
JSForVar JSNode JSNode JSNode [JSNode] JSNode [JSNode] JSNode [JSNode] JSNode JSNode
for,lb,var,vardecl,semi,expr,semi,expr,rb,stmt
JSForVarIn JSNode JSNode JSNode JSNode JSNode JSNode JSNode JSNode
for,lb,var,vardecl,in,expr,rb,stmt
JSMemberSquare [JSNode] JSNode JSNode JSNode
firstpart, lb, expr, rb
JSOperator JSNode
opnode
JSPropertyAccessor JSNode JSNode JSNode [JSNode] JSNode JSNode
(get|set), name, lb, params, rb, block
JSSwitch JSNode JSNode JSNode JSNode JSNode
switch,lb,expr,rb,caseblock
JSThrow JSNode JSNode
throw val
JSTry JSNode JSNode [JSNode]
try,block,rest
JSUnary String JSNode
type, operator
 -}
