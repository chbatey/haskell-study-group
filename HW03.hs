module HW03 where

data Expression =
    Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop = 
    Plus     
  | Minus    
  | Times    
  | Divide   
  | Gt
  | Ge
  | Lt  
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement       
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement        
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Exercise 1 -----------------------------------------

extend :: State -> String -> Int -> State
extend state str i str2
    | str2 == str = i
    | otherwise = state str2

empty :: State
empty _ = 0

-- Exercise 2 -----------------------------------------

evalE :: State -> Expression -> Int
evalE state (Var s) = state s
evalE _ (Val i) = i
evalE state (Op e1 b e2) = bop b (evalE state e1) (evalE state e2)

bop :: Bop -> Int -> Int -> Int
bop Plus a b = a + b
bop Minus a b = a - b
bop Times a b = a * b
bop Divide a b = a `div` b
bop Gt a b = toInt (a > b)
bop Ge a b = toInt (a >= b)
bop Lt a b = toInt (a < b)
bop Le a b = toInt (a <= b)
bop Eql a b = toInt (a == b)

toInt :: Bool -> Int
toInt True = 1
toInt False = 0


-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar Skip = DSkip
desugar (Sequence s1 s2) = DSequence (desugar s1) (desugar s2)
desugar (While e s) = DWhile e (desugar s)
desugar (If e s1 s2) = DIf e (desugar s1) (desugar s2)
desugar (Assign str e) = DAssign str e
desugar (Incr str) = DAssign str (Op (Var str) Plus (Val 1))
desugar (For s1 e s2 s3) = DSequence (desugar s1) (DWhile e (DSequence (desugar s2) (desugar s3)))


-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple state (DAssign str e) = extend state str (evalE state e)

evalSimple state (DIf e st1 st2) = evalSimple state toEval
    where toEval = if evalE state e == 1 then st1 else st2

evalSimple state w@(DWhile e st) = if shouldEval then
                                    evalSimple (evalSimple state st) w
                                  else state
    where shouldEval = evalE state e == 1

evalSimple state (DSequence st1 st2) = evalSimple nextState st2
    where nextState = evalSimple state st1

evalSimple state DSkip = state

run :: State -> Statement -> State
run state st = evalSimple state $ desugar st

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

simples :: Statement
simples = slist [ Assign "A" (Val 4), If (Val 1) (Incr "In") (Incr "In2")]

whiles :: Statement
whiles = undefined
-- whiles :: While (Op (Var "In") Lt (Val 5)) (Sequence (Incr "In") (Incr "c"))

{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Calculate the floor of the square root of the input

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]
