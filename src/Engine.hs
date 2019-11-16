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
    renameRel = toRel . renameTerm . toFunctor
    toRel (Func name terms) = Rel name terms
    renameTerm (Var v)            = Var (v ++ show height)
    renameTerm (Func name terms)  = Func name (map renameTerm terms)
    renameTerm atom               = atom

toFunctor :: Rel -> Term
toFunctor (Rel name terms) = Func name terms

match :: Rel -> Rule -> Bool
match (Rel name terms) (Rule (Rel name' terms') _) =
  name == name' && length terms == length terms'


searchAll :: Program -> Tree -> [Subs]
searchAll program tree =
  error "to be implemented"


{- returns all variables in a relation -}
variables :: Rel -> [Term]
variables rel = S.toList . S.fromList . aux . toFunctor $ rel
  where
    aux v@(Var x) = [v]
    aux (Func _ terms) = terms >>= aux
    aux (Atom _) = []

{- apply substitution to a term -}
resolve :: Subs -> Term -> Term
resolve subs (Var x) =
  case M.lookup x subs of
    Just term -> resolve subs term
    Nothing -> Var x

resolve subs (Func name terms) = Func name (map (resolve subs) terms)
resolve subs a@(Atom _) = a