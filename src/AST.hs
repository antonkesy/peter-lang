{-# LANGUAGE GADTs #-}

module AST (module AST) where

type Name = String

data Operator
  = Plus
  | Minus
  | Multiply
  | Divide
  | Modulus
  | And
  | Or
  | Not
  | Eq
  | Neq
  | Lt
  | Gt
  | Le
  | Ge
  deriving (Show, Eq)

data Literal
  = IntLiteral Int
  | FloatLiteral Float
  | BoolLiteral Bool
  | UnitLiteral
  | StringLiteral String
  | UndefinedLiteral
  deriving (Show, Eq)

data Atomic
  = LiteralAtomic Literal
  | VariableAtomic Name
  | FunctionCallAtomic Name [Expression]
  deriving (Show, Eq)

data Expression
  = OperationExpression Expression Operator Expression
  | AtomicExpression Atomic
  deriving (Show, Eq)

data VariableDeclaration = VariableDeclaration Name Type
  deriving (Show, Eq)

data Variable = Variable VariableDeclaration Expression
  deriving (Show, Eq)

data Assignment = Assignment Name Expression
  deriving (Show, Eq)

data Type
  = IntType
  | FloatType
  | BoolType
  | UnitType
  | CustomType Name
  | StringType
  | UndefinedType
  deriving (Show, Eq)

data Control
  = IfControl Expression [Statement] (Maybe [Statement])
  | WhileControl Expression [Statement]
  deriving (Show, Eq)

data Struct = Struct Name [VariableDeclaration]
  deriving (Show, Eq)

data Statement
  = VariableDefinitionStatement Variable
  | AssignmentStatement Assignment
  | FunctionDefinitionStatement Function
  | ExpressionStatement Expression
  | ReturnStatement Expression
  | ControlStatement Control
  | StructStatement Struct
  | VariableDeclarationStatement VariableDeclaration
  deriving (Show, Eq)

data Function = Function Name [VariableDeclaration] Type [Statement]
  deriving (Show, Eq)

data Program where
  Program :: [Statement] -> Program
  deriving (Show, Eq)

data BuiltInFuction = Print | Input
  deriving (Show, Eq)
