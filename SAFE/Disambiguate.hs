module Disambiguate (
  disambiguate
  ) where

import PGF
import SAFEQuery
import Data.Maybe
import Data.Data (Data)
import Data.Data.Lens (template)
import Control.Lens (toListOf, over, Traversal')
import Debug.Trace (trace)

disambiguate :: Tree -> [Tree]
disambiguate = map gf . disambiguateMove . fg

-- For each ambiguous term in the Move, return another Move
disambiguateMove :: Move -> [Move]
disambiguateMove = mapMaybe expandRelative . findTerms

findTerms :: Move -> [Term]
findTerms move =
  toListOf termsInKindsInTerms move ++ toListOf termsIn move
 where
  -- Giving names and type signatures to template
  termsInKindsInTerms :: Traversal' Move Term
  termsInKindsInTerms = termsIn . kindsInTerm . termsIn

  termsIn :: (Data s) => Traversal' s Term
  termsIn = template

  kindsInTerm :: Traversal' Term Kind
  kindsInTerm = template

{- The most important function: expands relative clauses like
   "a contract, under which the Company sells stock"
 → "the Company sells stock under a contract" -}
expandRelative :: Term -> Maybe Move
expandRelative term = case term of
 RelDir dobj subj tns vpslash
   -> Just $ simplifyTerms $
      MAction tns subj (AComplDir vpslash dobj)
 RelIndir iobj subj tns vpslash
   -> Just $ simplifyTerms $
      MAction tns subj (AComplIndir vpslash iobj)
 x ->  Nothing -- Term doesn't have a relative clause
-- x -> trace (show x) Nothing -- debug: show the term

simplifyTerms :: Move -> Move
simplifyTerms = over template onlyHead
 where
  onlyHead :: Term -> Term
  onlyHead term = case term of
    TExcluding det kind _
      -> TDet det (rmAdjuncts kind)
    TIncluding det kind _
      -> TDet det (rmAdjuncts kind)
    _ -> over template rmAdjuncts term

  rmAdjuncts :: Kind -> Kind
  rmAdjuncts kind = case kind of
    KProperty _ k -> rmAdjuncts k
    KWhetherOr _ k -> rmAdjuncts k
    SingleOrSeries k -> rmAdjuncts k
    _ -> kind
