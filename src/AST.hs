module AST (module AST) where

type Name = String

data Operator = Plus | Minus | Multiply | Divide | Modulus | And | Or | Not | Eq | Neq | Lt | Gt | Le | Ge
  deriving (Show, Eq)

data Literal = IntLiteral Int | FloatLiteral Float | BoolLiteral Bool | UnitLiteral
  deriving (Show, Eq)

data Atomic = LiteralAtomic Literal | VariableAtomic Name
  deriving (Show, Eq)

data Expression = OperationExpression Expression Operator Expression | AtomicExpression Atomic
  deriving (Show, Eq)

data Variable = Variable Name Type Expression
  deriving (Show, Eq)

data Assignment = Assignment Name Expression
  deriving (Show, Eq)

type Comment = String

data Type = IntType | FloatType | BoolType | UnitType | CustomType Name
  deriving (Show, Eq)

data Statement = VariableStatement Variable | AssignmentStatement Assignment
  deriving (Show, Eq)

-- data Function = Function Name [Variable] Type [Statement]
-- deriving (Show)

-- data Program = Program {main :: Function, restFunctions :: [Function]}
data Program = Program [Statement]
  deriving (Show, Eq)

-- TODO: builtin: print, input
