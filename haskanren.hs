module MicroHasKanren where

newtype Value = Num Integer deriving(Show, Eq)
data VarVal = Var Integer | Val Value deriving(Show, Eq)
type Substitution = (VarVal, VarVal)
type State = ([Substitution], Integer)
type Goal = State -> [State]

emptySubstitutions :: [Substitution]
emptySubstitutions = []

emptyState :: State
emptyState = (emptySubstitutions, 0)

extSubstitutions :: VarVal -> VarVal -> [Substitution] -> [Substitution]
extSubstitutions u v s = (u, v) : s

assoc :: VarVal -> [Substitution] -> Maybe VarVal
assoc _ [] = Nothing
assoc k (h:s') = if k == fst h
  then Just $ snd h
  else assoc k s'

walk :: VarVal -> [Substitution] -> VarVal
walk val@(Val _) _ = val
walk v@(Var _) s =
  case assoc v s of
    Just u -> walk u s
    Nothing -> v

unify :: VarVal -> VarVal -> [Substitution] -> Maybe [Substitution]
unify u v s
  | u == v = Just s
unify u@(Var _) v s = Just $ extSubstitutions u v s
unify u v@(Var _) s = Just $ extSubstitutions v u s
unify _ _ _ = Nothing

unit :: State -> [State]
unit (s,c) = [(s,c)]

(~~) :: VarVal -> VarVal -> Goal
(~~) u v (s, c) = case unify u v s of
  Just s' -> unit (s', c)
  Nothing -> []

callFresh :: (VarVal -> Goal) -> Goal
callFresh f (s, c) = f (Var c) (s, c + 1)

disj :: Goal -> Goal -> Goal
disj g1 g2 stream = mplus (g1 stream) (g2 stream)

mplus :: [State] -> [State] -> [State]
mplus [] stream = stream
mplus (state : stream1') stream2 = state : mplus stream1' stream2

conj :: Goal -> Goal -> Goal
conj g1 g2 stream = bind (g1 stream) g2

bind :: [State] -> Goal -> [State]
bind stream g = foldr (mplus . g) [] stream

{- {- Examples -}
x = Var 0
y = Var 1
zero = Val (Num 0)
one = Val (Num 1)
two = Val (Num 2)
three = Val (Num 3)
st :: State
st = ([(x,y), (y,one)], 2)
test1 :: [State]
test1 = callFresh (\ v -> v ~~ Val (Num 5)) emptyState
test2 :: [State]
test2 = callFresh (\ v -> (callFresh (\u -> v ~~ u))) emptyState
test3 :: [State]
test3 =
  conj
  (callFresh (\ a -> a ~~ zero))
  (callFresh (\ b -> disj (b ~~ one) (b ~~ two) ) ) emptyState
-}
