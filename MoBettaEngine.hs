module MoBettaEngine where

import System.IO
import qualified Data.Map as HM
import Control.Monad.State
import Control.Applicative
import Data.Maybe (fromMaybe)

import MoBettaAST



type Env = HM.Map String Integer
-- Creating an empty environment
emptyEnv :: Env
emptyEnv = HM.fromList []


type Computation t = StateT Env IO t

-- For clarity we declare
--  Action : a computation corresponding to a statement
--  IntCalc : a computation that produces and Int
--  BoolCalc

type Action = Computation ()
type IntCalc = Computation Integer
type BoolCalc = Computation Bool

-- Defining a translator from Statements to Actions
statementAction :: Statement -> Action
statementAction (Print e) = printAction (intCalc e)
statementAction (Msg s) = msgAction s -- displays a string s
statementAction (Read v) = readAction v -- get a value inputted for v
statementAction (If b s1 s2) =
  ifAction (boolCalc b) (statementAction s1) (statementAction s2)
statementAction (While b s) = whileAction (boolCalc b) (statementAction s)
statementAction (Assign v e) = assignAction v (intCalc e)
statementAction (Block ls) = makeProgram ls
  -- compute a sequence of lines by translating each into a computation

makeProgram ls = blockAction $ map statementAction ls

{---------------------------------------------------------------------------
Some helpers to manipulate the state and to access IO.
----------------------------------------------------------------------------}

-- In a Computation block, write "doIO print" instead of "print", etc.
doIO :: IO a -> Computation a
doIO = lift

-- updateEnv is used to modify or insert variables
updateEnv :: String -> Integer -> Computation ()
updateEnv name val = modify $ HM.insert name val

-- retrieveEnv gets environment variable values
-- HM.lookup can fail if the identifier we are trying to retrieve does not exist. So "val" is a "Maybe Int" -- "fromMaybe" is a simple way to deal with Maybe failures.
retrieveEnv :: String -> Computation Integer
retrieveEnv name = do
  val <- gets $ HM.lookup name
  return $ fromMaybe (varNotFound name) val
  where
    varNotFound name = error $ "Identifier \"" ++ name ++ "\" not defined."


{---------------------------------------------------------------------------
Now to interpret individual statement types
----------------------------------------------------------------------------}

-- Read and store an integer in a variable
readAction :: String -> Action
readAction v = do
  x <- doIO getInt
  updateEnv v x
  where
    getInt = do
      inp <- getLine
      return $ read inp

-- Display a string
msgAction :: String -> Action
msgAction s = doIO $ putStr s

-- Display result of computing an integer
printAction :: IntCalc -> Action
printAction intCalc = do
  i <- intCalc
  doIO $ putStr $ show i

-- Compute an integer, then store it
assignAction :: String -> IntCalc -> Action
assignAction v intCalc = do
  i <- intCalc
  updateEnv v i

-- Compute a boolean, use it to decide which computation to do.
ifAction :: BoolCalc -> Action -> Action -> Action
ifAction boolCalc action1 action2 = do
  b <- boolCalc
  if b then
    action1
  else
    action2

whileAction :: BoolCalc -> Action -> Action
whileAction boolCalc action = do
  b <- boolCalc
  if b
    then do
      action
      whileAction boolCalc action
  else
    return ()

-- Do a list of actions sequentially.
blockAction :: [Action] -> Action
blockAction [] = return ()
blockAction (a:ls) = do
  a
  blockAction ls

binOpMap = [
    (Add, (+)), -- Int ops
    (Sub, (-)),
    (Mul, (*)),
    (Div, div),
    (Mod, mod)
  ]
unOpMap = [
    (Neg, negate)
  ]

compOpMap = [
    (Greater, (>)),
    (GreaterEqual, (>=)),
    (Less, (<)),
    (LessEqual, (<=)),
    (Equal, (==)),
    (NEqual, (/=))
  ]
binBoolOpMap = [
    (And, (&&)), -- Bool ops
    (Or, (||))
  ]

unBoolOpMap = [
    (Not, not) ]

-- opLookup :: Eq const => const -> [(const, sem)] -> sem
-- opLookup op table = lookup op table
-- The above code returns a maybe, so we need to unwrap it
opLookup :: Eq const => const -> [(const, sem)] -> sem
opLookup op table = fromMaybe (error "invalid op") (lookup op table)

-- This defines the translation of a BExpr into a computation of Booleans
boolCalc :: BExpr -> BoolCalc
boolCalc (BoolConst b) = return b
boolCalc (Reln cOp expr1 expr2) =
  liftA2 (opLookup cOp compOpMap) (intCalc expr1) (intCalc expr2)
boolCalc (BBin op expr1 expr2) =
  liftA2 (opLookup op binBoolOpMap) (boolCalc expr1) (boolCalc expr2)
boolCalc (BUn op expr) =
  fmap (opLookup op unBoolOpMap) (boolCalc expr)

intCalc :: AExpr -> IntCalc
intCalc (Var v) = retrieveEnv v
intCalc (IntConst val) = return val
intCalc (ABin op expr1 expr2) =
  liftA2 (opLookup op binOpMap) (intCalc expr1) (intCalc expr2)
intCalc (AUn op expr) =
  fmap (opLookup op unOpMap) (intCalc expr)
