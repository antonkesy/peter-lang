module AST (module AST) where

type Name = String

data Operator = Plus | Minus | Times | Div | Mod | And | Or | Not | Eq | Neq | Lt | Gt | Le | Ge
  deriving (Show)

data Literal = IntLiteral Int | FloatLiteral Float | BoolLiteral Bool | UnitLiteral
  deriving (Show)

data Expression = Operation Expression Operator Expression | Atomic Literal
  deriving (Show)

data Variable = Variable Name Type Expression
  deriving (Show)

type CustomeType = Name

type Comment = String

data Type = Int | Float | Bool | Unit | CustomeType
  deriving (Show)
