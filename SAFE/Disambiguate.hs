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
disambiguateMove :: GMove -> [GMove]
disambiguateMove = mapMaybe expandRelative . findTerms

findTerms :: GMove -> [GTerm]
findTerms move = -- concatMap termsInKinds kinds ++ terms
  toListOf termsInKindsInTerms move ++ toListOf termsIn move
 where
  -- terms = termsInMove move
  -- kinds = concatMap kindsInTerm terms

  termsInKindsInTerms :: Traversal' GMove GTerm
  termsInKindsInTerms = termsIn . kindsInTerm . termsIn

  -- Giving names to these, confusing with "toListOf template" everywhere
  termsIn :: (Data s) => Traversal' s GTerm
  termsIn = template

  kindsInTerm :: Traversal' GTerm GKind
  kindsInTerm = template

-- termsInMove :: GMove -> [GTerm]
-- termsInMove = toListOf template

-- termsInKind :: GKind -> [GTerm]
-- termsInKind = toListOf template

-- kindsInTerm :: GTerm -> [GKind]
-- kindsInTerm = toListOf template

{- The most important function: expands relative clauses like
   "a contract, under which the Company sells stock"
 → "the Company sells stock under a contract" -}
expandRelative :: GTerm -> Maybe GMove
expandRelative term = case term of
 GRelDir dobj subj tns vpslash
   -> Just $ simplifyTerms $
      GMAction tns subj (GAComplDir vpslash dobj)
 GRelIndir iobj subj tns vpslash
   -> Just $ simplifyTerms $
      GMAction tns subj (GAComplIndir vpslash iobj)
 x ->  Nothing -- Term doesn't have a relative clause
-- x -> trace (show x) Nothing -- debug: show the term

simplifyTerms :: GMove -> GMove
simplifyTerms = over template onlyHead
 where
  onlyHead :: GTerm -> GTerm
  onlyHead term = case term of
    GTExcluding det kind _
      -> GTDet det (rmAdjuncts kind)
    GTIncluding det kind _
      -> GTDet det (rmAdjuncts kind)
    _ -> over template rmAdjuncts term

  rmAdjuncts :: GKind -> GKind
  rmAdjuncts kind = case kind of
    GKProperty _ k -> rmAdjuncts k
    GKWhetherOr _ k -> rmAdjuncts k
    GSingleOrSeries k -> rmAdjuncts k
    _ -> kind

{-
{-# LANGUAGE DeriveDataTypeable #-}

module SAFEQuery where

import PGF hiding (Tree)
import Data.Data

-}
-- $>
