module Language.Mulang.Inspector.Smell (
  hasRedundantBooleanComparison,
  hasRedundantIf,
  hasRedundantGuards,
  hasRedundantLambda,
  hasRedundantParameter,
  hasUnifyOperator,
  hasOrOperator,
  doesNullTest,
  doesTypeTest,
  returnsNull) where

import Language.Mulang
import Language.Mulang.Inspector


-- | Inspection that tells whether a binding has expressions like 'x == True'
hasRedundantBooleanComparison :: Inspection
hasRedundantBooleanComparison = compares isBooleanLiteral

doesNullTest :: Inspection
doesNullTest = compares f
  where f MuNull = True
        f _      = False

doesTypeTest :: Inspection
doesTypeTest = compares f
  where f (MuString _) = True
        f _            = False

compares :: (Expression -> Bool) -> Inspection
compares f = containsExpression (any f.comparisonOperands)

comparisonOperands (Application Equal    args) = args
comparisonOperands (Application NotEqual args) = args
comparisonOperands _ = []

returnsNull :: Inspection
returnsNull = containsExpression f
  where f (Return MuNull) = True
        f _               = False

-- | Inspection that tells whether a binding has an if expression where both branches return
-- boolean literals
hasRedundantIf :: Inspection
hasRedundantIf = containsExpression f
  where f (If _ (Return x) (Return y)) = all isBooleanLiteral [x, y]
        f (If _ x y)                   = all isBooleanLiteral [x, y]
        f _                            = False

-- | Inspection that tells whether a binding has guards where both branches return
-- boolean literals
hasRedundantGuards :: Inspection
hasRedundantGuards = containsBody f -- TODO not true when condition is a pattern
  where f (GuardedBody [
            (_, Return x),
            (Variable "otherwise", Return y)]) = all isBooleanLiteral [x, y]
        f _ = False


-- | Inspection that tells whether a binding has lambda expressions like '\x -> g x'
hasRedundantLambda :: Inspection
hasRedundantLambda = containsExpression f
  where f (Lambda [VariablePattern (x)] (Return (Application _ [Variable (y)]))) = x == y
        f _ = False


-- | Inspection that tells whether a binding has parameters that
-- can be avoided using point-free
hasRedundantParameter :: Inspection
hasRedundantParameter = containsExpression f
  where f (FunctionDeclaration _ [Equation params (UnguardedBody (Return (Application _ args)))])
                                                            | (VariablePattern param) <- last params,
                                                              (Variable arg) <- last args = param == arg
        f _ = False

isBooleanLiteral (MuBool _) = True
isBooleanLiteral _          = False

hasUnifyOperator :: Inspection
hasUnifyOperator = containsExpression f
  where f (Exist "=" _) = True
        f _ = False

hasOrOperator :: Inspection
hasOrOperator = containsExpression f
  where f (OrOperator _ _) = True
        f _ = False
