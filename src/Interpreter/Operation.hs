module Interpreter.Operation (module Interpreter.Operation) where

import AST
import Interpreter.ProgramState

interpretOperation :: Operator -> Value -> Value -> Value
-- Int
interpretOperation Plus (IntValue left) (IntValue right) = IntValue $ left + right
interpretOperation Minus (IntValue left) (IntValue right) = IntValue $ left - right
interpretOperation Multiply (IntValue left) (IntValue right) = IntValue $ left * right
interpretOperation Divide (IntValue left) (IntValue right) = IntValue $ left `div` right
interpretOperation Lt (IntValue left) (IntValue right) = BoolValue $ left < right
interpretOperation Gt (IntValue left) (IntValue right) = BoolValue $ left > right
interpretOperation Le (IntValue left) (IntValue right) = BoolValue $ left <= right
interpretOperation Ge (IntValue left) (IntValue right) = BoolValue $ left >= right
interpretOperation Eq (IntValue left) (IntValue right) = BoolValue $ left == right
-- Float
interpretOperation Plus (FloatValue left) (FloatValue right) = FloatValue $ left + right
interpretOperation Minus (FloatValue left) (FloatValue right) = FloatValue $ left - right
interpretOperation Multiply (FloatValue left) (FloatValue right) = FloatValue $ left * right
interpretOperation Lt (FloatValue left) (FloatValue right) = BoolValue $ left < right
interpretOperation Gt (FloatValue left) (FloatValue right) = BoolValue $ left > right
interpretOperation Le (FloatValue left) (FloatValue right) = BoolValue $ left <= right
interpretOperation Ge (FloatValue left) (FloatValue right) = BoolValue $ left >= right
interpretOperation Eq (FloatValue left) (FloatValue right) = BoolValue $ left == right
-- String
interpretOperation Plus (StringValue left) (StringValue right) = StringValue $ left ++ right
-- Invalid
interpretOperation operator left right = error $ "Unsupported operation: " ++ show operator ++ " " ++ show left ++ " " ++ show right
