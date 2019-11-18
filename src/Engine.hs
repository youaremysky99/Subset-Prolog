module Engine where

import AST
import Control.Monad
import qualified Data.Map.Strict as M
import qualified Data.Set as S

data Goal = Goal Subs [Rel]
data Tree = Tree Goal [Tree] Bool -- isExpanded
 {- this represent the resolution tree -}
type Subs = M.Map String Term
 {- this represents a substitution -}

{- an initial resolution tree -}
initTree :: Rel -> Tree
initTree rel = Tree (Goal M.empty [rel]) [] False

{- unifying two terms -}
unify :: Term -> Term -> Subs -> Maybe Subs
unify (Atom a1) (Atom a2) subs = if a1 == a2 then Just subs else Nothing
unify (Func n1 t1) (Func n2 t2) subs =
  if n1 == n2
    then foldl (>=>) Just (zipWith unify t1 t2) subs
    else Nothing
unify (Var v) term subs =
  case subs M.!? v of
    Just term' -> unify term' term subs
    Nothing -> Just (M.insert v term subs)
unify term (var@(Var v)) subs = unify var term subs
unify _ _ _ = Nothing

{- renaming a clause -}
rename :: Rule -> Int -> Rule
rename (Rule head body) height = Rule (renameRel head) (map (map renameRel) body)
  where
    renameRel = \x -> 
      case x of
        Cut -> x
        _ -> (toRel . renameTerm . toFunctor) x
    toRel (Func name terms) = Rel name terms
    renameTerm (Var v)            = Var (v ++ show height)
    renameTerm (Func name terms)  = Func name (map renameTerm terms)
    renameTerm atom               = atom

toFunctor :: Rel -> Term
toFunctor (Rel name terms) = Func name terms

match :: Rel -> Rule -> Bool
match (Rel name terms) (Rule (Rel name' terms') _) =
  name == name' && length terms == length terms'


searchAll :: Program -> Tree -> Int -> [Subs]
searchAll prog@(Program rules) (Tree goal@(Goal subs rels) subtree exp) height =
  case rels of
    [] -> return subs
    (Cut : _) -> return subs
    _ -> 
      case exp of
        False -> searchAll prog (Tree goal (expandTree prog goal subtree (height+1)) True) height
        True ->
          case subtree of
            [] -> []
            lst -> do
              stree <- lst
              searchAll prog stree (height+1)

expandTree :: Program -> Goal -> [Tree] -> Int -> [Tree]
expandTree (Program rules) (Goal subs (rel : rest)) trees height = do
  matched <- filter (match rel) rules
  let newRule = rename matched height
  let clause = getClause newRule
  let lst = getRelList newRule
  case unify (toFunctor clause) (toFunctor rel) subs of
    Just newSub -> return (Tree (Goal newSub (lst ++ rest)) [] False)
    Nothing -> []

getClause :: Rule -> Rel
getClause (Rule rel _) = rel

getRelList :: Rule -> [Rel]
getRelList (Rule _ rels) = (rels >>= (\x -> x))


{- returns all variables in a relation -}
variables :: Rel -> [Term]
variables rel = S.toList . S.fromList . aux . toFunctor $ rel
  where
    aux v@(Var x) = [v]
    aux (Func _ terms) = terms >>= aux
    aux (Atom _) = []

variablesOfList :: [Rel] -> [Term]
variablesOfList [] = []
variablesOfList (x: xs) = variables(x) ++ variablesOfList(xs)

variablesOfQuery :: [[Rel]] -> [Term]
variablesOfQuery [] = []
variablesOfQuery (x: xs) = variablesOfList(x) ++ variablesOfQuery(xs)


{- apply substitution to a term -}
resolve :: Subs -> Term -> Term
resolve subs (Var x) =
  case M.lookup x subs of
    Just term -> resolve subs term
    Nothing -> Var x

resolve subs (Func name terms) = Func name (map (resolve subs) terms)
resolve subs a@(Atom _) = a

append :: Program -> Rule -> Program 
append (Program rules) new_rule = Program (new_rule: rules) 

