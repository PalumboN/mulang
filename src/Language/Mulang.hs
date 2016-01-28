{-# LANGUAGE DeriveGeneric #-}

module Language.Mulang (
    Declaration(..),
    Equation(..),
    Rhs(..),
    GuardedRhs(..),
    Expression(..),
    ComprehensionStatement(..),
    Alternative(..),
    GuardedAlternatives(..),
    GuardedAlternative(..),
    Pattern(..),
    LiteralValue(..)
  ) where

import           GHC.Generics

type Identifier = String

-- declaration or directive
data Declaration
         = TypeAlias Identifier
         | RecordDeclaration Identifier
         | TypeSignature Identifier
         | FunctionDeclaration Identifier [Equation]    -- functional, maybe pure,
                                                        -- optionally guarded,
                                                        -- optionally pattern matched function
         | ProcedureDeclaration Identifier              -- classic imperative-style procedure
         | ConstantDeclaration Identifier Rhs
  deriving (Eq, Show, Read, Generic)

data Equation = Equation [Pattern] Rhs deriving (Eq, Show, Read, Generic)

data Rhs
         = UnguardedRhs Expression
         | GuardedRhss  [GuardedRhs]
  deriving (Eq, Show, Read, Generic)

data GuardedRhs = GuardedRhs Expression Expression deriving (Eq, Show, Read, Generic)

-- expression or statement
-- may have effects
data Expression
        = DeclarationExpression Declaration
        | Variable Identifier
        | Literal LiteralValue
        | InfixApplication Expression String Expression
        | Application Expression Expression
        | Lambda [Pattern] Expression
        | Let [Declaration] Expression
        | If Expression Expression Expression
        | Match Expression [Alternative]
        | MuTuple [Expression]
        | MuList [Expression]
        | Comprehension Expression [ComprehensionStatement]
        | Sequence [Expression]                   -- sequence of statements
        | ExpressionOther
  deriving (Eq, Show, Read, Generic)

data Pattern
        = VariablePattern String                 -- ^ variable
        | LiteralPattern String              -- ^ literal constant
        | InfixApplicationPattern Pattern String Pattern
        | ApplicationPattern String [Pattern]        -- ^ data constructor and argument
        | TuplePattern [Pattern]              -- ^ tuple pattern
        | ListPattern [Pattern]               -- ^ list pattern
        | AsPattern String Pattern         -- ^ @\@@-pattern
        | WildcardPattern                   -- ^ wildcard pattern (@_@)
        | OtherPattern
  deriving (Eq, Show, Read, Generic)

data ComprehensionStatement
        = MuGenerator Pattern Expression
        | MuQualifier Expression
        | LetStmt [Expression]
  deriving (Eq, Show, Read, Generic)


data Alternative = Alternative Pattern GuardedAlternatives [Expression] deriving (Eq, Show, Read, Generic)

data GuardedAlternatives
        = UnguardedAlternative Expression          -- ^ @->@ /exp/
        | GuardedAlternatives  [GuardedAlternative] -- ^ /gdpat/
  deriving (Eq, Show, Read, Generic)


data GuardedAlternative = GuardedAlternative Expression Expression deriving (Eq, Show, Read, Generic)

data LiteralValue
          = MuBool Bool
          | MuInteger Integer
          | MuFloat Double
          | MuString String
    deriving (Eq, Show, Read, Generic)
