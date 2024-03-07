module AST (module AST) where

type Name = String

data Operator = Plus | Minus | Multiply | Divide | Modulus | And | Or | Not | Eq | Neq | Lt | Gt | Le | Ge
  deriving (Show, Eq)

data Literal = IntLiteral Int | FloatLiteral Float | BoolLiteral Bool | UnitLiteral | StringLiteral String
  deriving (Show, Eq)

data Atomic = LiteralAtomic Literal | VariableAtomic Name | FunctionCallAtomic Name [Expression]
  deriving (Show, Eq)

data Expression = OperationExpression Expression Operator Expression | AtomicExpression Atomic
  deriving (Show, Eq)

data VariableDeclaration = VariableDeclaration Name Type
  deriving (Show, Eq)

data Variable = Variable VariableDeclaration Expression
  deriving (Show, Eq)

data Assignment = Assignment Name Expression
  deriving (Show, Eq)

type Comment = String

data Type = IntType | FloatType | BoolType | UnitType | CustomType Name | StringType
  deriving (Show, Eq)

data Statement = VariableStatement Variable | AssignmentStatement Assignment | FunctionDefinitionStatement Function | ExpressionStatement Expression
  deriving (Show, Eq)

data Function = Function Name [VariableDeclaration] Type [Statement]
  deriving (Show, Eq)

data Program = Program [Statement]
  deriving (Show, Eq)

data BuiltInFuction = Print | Input
  deriving (Show, Eq)
