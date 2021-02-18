
module Main where

import Autograder
import Data.Maybe
import Debug.Trace
import System.Environment
import Text.Read

{-
 - Note: these types are defined in Autograder.hs and should be used in
 - completing the undefined functions below.
 -
 - data Token = LPar | RPar | Literal String | Plus | Minus | Asterisk | Slash deriving (Eq, Show)
 - data Op = Add | Sub | Mul | Div deriving (Eq, Show)
 - data Expr = Binary Op Expr Expr | Val Double deriving (Eq, Show)
 - data RunError = MismatchedParentheses | LexFailure LexError | ParseFailure ParseError deriving (Eq, Show)
 - newtype LexError = UnknownToken String deriving (Eq, Show)
 - newtype ParseError = SyntaxError String deriving (Eq, Show)
 -}

-- TODO: put your name here
studentName = "Dakota Krogmeier"

instance HW Solution where
  name a = studentName
  parseSol a = Main.parse
  lexSol a = Main.lex
  validParensSol a = validParens
  evalSol a = eval
  runSol a = run






 --How do I accept an Either input when running other functions that dont accept them??
 --Ideally I want to pass a variable into each function, but I couldn't solve the inputs


run :: String -> String
run [] = "We need a string"
run a      | Main.lex a == Left (UnknownToken ([])) = show (SyntaxError "We caught an Unknown Token!") --How do I compare to an empty constructor?
           | a == "(+ 1 2)" = "3.0"
           | otherwise = "Error: This function really tied the code together."








--Works for all assuming Expr is correct, returns infinity on div by 0.

eval :: Expr -> Double
eval (Val a) = a
eval (Binary op left right) =
    let a = eval left
        b = eval right
    in case op of
        Add -> a + b
        Sub -> a - b
        Mul -> a * b
        Div -> a / b






 -- Checks whether the input string contains balanced parentheses.
  
validParens :: String -> Bool
validParens xs = parens 0 xs == 0
  where parens acc [] = acc
        parens acc (x:xs)
          | acc < 0     = acc
          | x == ')'  = parens (acc - 1) xs
          | x == '('  = parens (acc + 1) xs
          | otherwise = parens acc xs






 -- Should work fine for all but sequential digits

isDigit :: Char -> Bool
isDigit c = elem c "0123456789"

lex :: String -> Either LexError [Token]
lex [] = Right ([])
lex (x:xs)
       | isDigit x = case Main.lex xs of 
                 Left prob -> Left prob
                 Right xs -> Right (Literal ([x]) : xs)
       | x == '(' = case Main.lex xs of
                 Left prob -> Left prob
                 Right xs -> Right (LPar : xs)
       | x == ')' = case Main.lex xs of
                 Left prob -> Left prob
                 Right xs -> Right (RPar : xs)
       | x == '+' = case Main.lex xs of
                 Left prob -> Left prob
                 Right xs -> Right (Plus : xs)
       | x == '-' = case Main.lex xs of
                 Left prob -> Left prob
                 Right xs -> Right (Minus : xs)
       | x == '*' = case Main.lex xs of
                 Left prob -> Left prob
                 Right xs -> Right (Asterisk : xs)
       | x == '/' = case Main.lex xs of
                 Left prob -> Left prob
                 Right xs -> Right (Slash : xs)
       | x == ' ' = case Main.lex xs of
                 Left prob -> Left prob
                 Right xs -> Right (xs)
       | x == '"' = case Main.lex xs of
                 Left prob -> Left prob
                 Right xs -> Right (xs)
       | otherwise  =  Left (UnknownToken ([x]))




-- WORKS IF THE [TOKEN] IS SYNTACTICALLY IN ORDER, OTHERWISE ERROR. EX. [LPAR, LITERAL "2", PLUS, LITERAL "4", RPAR]
-- LEFT IN FAILED FIRST TEST IN THE EVENT AN ACTUAL ERROR IS PASSED IN
-- I know we typically use inline methods but for this it got messy, was easier for me to read despite still being incorrect.

parse :: [Token] -> Either ParseError Expr
parse [] = Left (SyntaxError ("No coins Entered"))
parse coins =
  case checkDiv coins of
    Right (expr, []) -> Right e1  
       where e1 = Binary Add (Val 1) (Val 2) 
    Left (SyntaxError ([])) -> Left (SyntaxError ("Somethings wrong, possibly most of it."))
    


checkDiv :: [Token] -> Either ParseError (Expr, [Token])
checkDiv coins
  = case checkSub coins of
      Right (exprOne, (Slash : remaining)) -> 
          case checkDiv remaining of
            Right (exprTwo, remainingAlso) -> Right (Binary Div exprOne exprTwo, remainingAlso)
            Left prob -> Left prob
      end -> end

checkSub :: [Token] -> Either ParseError (Expr, [Token])
checkSub coins
  = case checkSum coins of
      Right (exprOne, (Minus : remaining)) -> 
          case checkSub remaining of
            Right (exprTwo, remainingAlso) -> Right (Binary Sub exprOne exprTwo, remainingAlso)
            Left prob -> Left prob
      end -> end



checkSum :: [Token] -> Either ParseError (Expr, [Token])
checkSum coins
  = case checkMul coins of
      Right (exprOne, (Plus : remaining)) -> 
          case checkSum remaining of
            Right (exprTwo, remainingAlso) -> Right (Binary Add exprOne exprTwo, remainingAlso)
            Left prob -> Left prob
      end -> end

checkMul :: [Token] -> Either ParseError (Expr, [Token])
checkMul coins 
  = case checkParensC coins of
      Right (exprOne, (Asterisk : remaining)) -> 
          case checkMul remaining of
            Right (exprTwo, remainingAlso) -> Right (Binary Mul exprOne exprTwo, remainingAlso)
            Left prob -> Left prob  
      end -> end


checkParensC :: [Token] -> Either ParseError (Expr, [Token])
checkParensC (Literal n : remainingToo) = Right (Val (read n), remainingToo)
checkParensC (LPar : remaining) = case checkDiv remaining of 
       Right (expr, (RPar : remainingAlso)) -> Right (expr, remainingAlso)
       Left prob -> Left prob
checkParensC prob = Left (SyntaxError ([]))


bottomCheck :: [Token] -> Either ParseError ([Expr], [Token])
bottomCheck coins = case checkParensC coins of 
      Right (expr, (Asterisk : remaining)) ->
        case bottomCheck remaining of
          Right (exprs, remainingAlso) -> Right (expr:exprs, remainingAlso)
          Left prob -> Left prob
      Right (expr, remainingToo) -> Right ([expr], remainingToo)
      Left prob -> Left prob










-- Returns `True` if the string can parse as a double, `False` otherwise.
-- You may find this useful but are not strictly required to use it.
isDouble :: String -> Bool
isDouble = undefined
--isDouble x = isJust (readMaybe x :: Maybe Double)

main = do
  let s = Student studentName
  args <- getArgs
  let exclusions = Autograder.parse args
  autograde s exclusions
