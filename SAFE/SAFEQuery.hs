{-# LANGUAGE DeriveDataTypeable #-}

module SAFEQuery where

import PGF hiding (Tree)
import Data.Data
----------------------------------------------------
-- automatic translation from GF to Haskell
----------------------------------------------------

class Gf a where
  gf :: a -> Expr
  fg :: Expr -> a

newtype GString = GString String  deriving (Show,Data)

instance Gf GString where
  gf (GString x) = mkStr x
  fg t =
    case unStr t of
      Just x  ->  GString x
      Nothing -> error ("no GString " ++ show t)

newtype GInt = GInt Int  deriving (Show,Data)

instance Gf GInt where
  gf (GInt x) = mkInt x
  fg t =
    case unInt t of
      Just x  ->  GInt x
      Nothing -> error ("no GInt " ++ show t)

newtype GFloat = GFloat Double  deriving (Show,Data)

instance Gf GFloat where
  gf (GFloat x) = mkFloat x
  fg t =
    case unFloat t of
      Just x  ->  GFloat x
      Nothing -> error ("no GFloat " ++ show t)

----------------------------------------------------
-- below this line machine-generated
----------------------------------------------------

data GAction =
   GAComplDir GAction_Dir GTerm
 | GAComplIndir GAction_Indir GTerm
 | GAComplNoneDir GAction_Dir GListTerm
 | GAComplNoneIndir GAction_Indir GListTerm
 | GANeg GAction
 | GConjAction GConjunction GListAction
  deriving (Show,Data)

data GAction_Dir =
   GASlashIndir GAction_Dir_Indir GTerm
 | GConjSlashDir GConjunction GListAction_Dir
 | GIssue
 | GRaise
 | GSell
  deriving (Show,Data)

data GAction_Dir_Indir =
   GConjSlashDirIndir GConjunction GListAction_Dir_Indir
 | GIssueAt
 | GSellAt
  deriving (Show,Data)

data GAction_Indir =
   GASlashDir GAction_Dir_Indir GTerm
 | GConjSlashIndir GConjunction GListAction_Indir
 | GPursuantTo GAction
  deriving (Show,Data)

data GConjunction =
   GAnd
 | GOr
  deriving (Show,Data)

data GDeterminer =
   GAPl
 | GASg
 | GAll
 | GAny
 | GAnyOther
 | GThePl
 | GTheSg
  deriving (Show,Data)

data GKind =
   GCapital
 | GChangeOfControl
 | GComplKind GKind_Term GTerm
 | GDirectListing
 | GDissolutionEvent
 | GEquityFinancing
 | GEvent
 | GGeneralAssignment
 | GInitialPublicOffering
 | GKProperty GProperty GKind
 | GKWhetherOr GListProperty GKind
 | GLiquidityEvent
 | GPreferredStock
 | GSingleOrSeries GKind
 | GTermination
 | GTransaction
 | GValuation
  deriving (Show,Data)

data GKind_Term =
   GConjSlashTerm GConjunction GListKind_Term
 | GDissolution
 | GLiquidation
 | GWindingUp
  deriving (Show,Data)

newtype GListAction = GListAction [GAction] deriving (Show,Data)

newtype GListAction_Dir = GListAction_Dir [GAction_Dir] deriving (Show,Data)

newtype GListAction_Dir_Indir = GListAction_Dir_Indir [GAction_Dir_Indir] deriving (Show,Data)

newtype GListAction_Indir = GListAction_Indir [GAction_Indir] deriving (Show,Data)

newtype GListKind = GListKind [GKind] deriving (Show,Data)

newtype GListKind_Term = GListKind_Term [GKind_Term] deriving (Show,Data)

newtype GListProperty = GListProperty [GProperty] deriving (Show,Data)

newtype GListTerm = GListTerm [GTerm] deriving (Show,Data)

data GMove =
   GMAction GTemporality GTerm GAction
 | GMDefProp GKind GProperty
 | GMDefTerm GKind GTerm
 | GMNo
 | GMQuery GQuery
 | GMYes
  deriving (Show,Data)

data GProperty =
   GBonaFide
 | GConjProperty GConjunction GListProperty
 | GFixed
 | GForBenefit GTerm
 | GPNeg GProperty
 | GPostMoney
 | GPreMoney
 | GVoluntary
 | GWithPurpose GAction
  deriving (Show,Data)

data GQuery =
   GQWhetherProp GTerm GProperty
 | GQWhetherTerm GTerm GTerm
 | GQWhichProp GKind GProperty
 | GQWhichTerm GKind GTerm
  deriving (Show,Data)

data GTemporality =
   GTFuture
 | GTPast
 | GTPresent
  deriving (Show,Data)

data GTerm =
   GCompany
 | GConjTerm GConjunction GListTerm
 | GCreditors GTerm
 | GRelDir GTerm GTerm GTemporality GAction_Dir
 | GRelIndir GTerm GTerm GTemporality GAction_Indir
 | GTDet GDeterminer GKind
 | GTExcluding GDeterminer GKind GTerm
 | GTIncluding GDeterminer GKind GTerm
  deriving (Show,Data)


instance Gf GAction where
  gf (GAComplDir x1 x2) = mkApp (mkCId "AComplDir") [gf x1, gf x2]
  gf (GAComplIndir x1 x2) = mkApp (mkCId "AComplIndir") [gf x1, gf x2]
  gf (GAComplNoneDir x1 x2) = mkApp (mkCId "AComplNoneDir") [gf x1, gf x2]
  gf (GAComplNoneIndir x1 x2) = mkApp (mkCId "AComplNoneIndir") [gf x1, gf x2]
  gf (GANeg x1) = mkApp (mkCId "ANeg") [gf x1]
  gf (GConjAction x1 x2) = mkApp (mkCId "ConjAction") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "AComplDir" -> GAComplDir (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "AComplIndir" -> GAComplIndir (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "AComplNoneDir" -> GAComplNoneDir (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "AComplNoneIndir" -> GAComplNoneIndir (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "ANeg" -> GANeg (fg x1)
      Just (i,[x1,x2]) | i == mkCId "ConjAction" -> GConjAction (fg x1) (fg x2)


      _ -> error ("no Action " ++ show t)

instance Gf GAction_Dir where
  gf (GASlashIndir x1 x2) = mkApp (mkCId "ASlashIndir") [gf x1, gf x2]
  gf (GConjSlashDir x1 x2) = mkApp (mkCId "ConjSlashDir") [gf x1, gf x2]
  gf GIssue = mkApp (mkCId "Issue") []
  gf GRaise = mkApp (mkCId "Raise") []
  gf GSell = mkApp (mkCId "Sell") []

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "ASlashIndir" -> GASlashIndir (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "ConjSlashDir" -> GConjSlashDir (fg x1) (fg x2)
      Just (i,[]) | i == mkCId "Issue" -> GIssue
      Just (i,[]) | i == mkCId "Raise" -> GRaise
      Just (i,[]) | i == mkCId "Sell" -> GSell


      _ -> error ("no Action_Dir " ++ show t)

instance Gf GAction_Dir_Indir where
  gf (GConjSlashDirIndir x1 x2) = mkApp (mkCId "ConjSlashDirIndir") [gf x1, gf x2]
  gf GIssueAt = mkApp (mkCId "IssueAt") []
  gf GSellAt = mkApp (mkCId "SellAt") []

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "ConjSlashDirIndir" -> GConjSlashDirIndir (fg x1) (fg x2)
      Just (i,[]) | i == mkCId "IssueAt" -> GIssueAt
      Just (i,[]) | i == mkCId "SellAt" -> GSellAt


      _ -> error ("no Action_Dir_Indir " ++ show t)

instance Gf GAction_Indir where
  gf (GASlashDir x1 x2) = mkApp (mkCId "ASlashDir") [gf x1, gf x2]
  gf (GConjSlashIndir x1 x2) = mkApp (mkCId "ConjSlashIndir") [gf x1, gf x2]
  gf (GPursuantTo x1) = mkApp (mkCId "PursuantTo") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "ASlashDir" -> GASlashDir (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "ConjSlashIndir" -> GConjSlashIndir (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "PursuantTo" -> GPursuantTo (fg x1)


      _ -> error ("no Action_Indir " ++ show t)

instance Gf GConjunction where
  gf GAnd = mkApp (mkCId "And") []
  gf GOr = mkApp (mkCId "Or") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "And" -> GAnd
      Just (i,[]) | i == mkCId "Or" -> GOr


      _ -> error ("no Conjunction " ++ show t)

instance Gf GDeterminer where
  gf GAPl = mkApp (mkCId "APl") []
  gf GASg = mkApp (mkCId "ASg") []
  gf GAll = mkApp (mkCId "All") []
  gf GAny = mkApp (mkCId "Any") []
  gf GAnyOther = mkApp (mkCId "AnyOther") []
  gf GThePl = mkApp (mkCId "ThePl") []
  gf GTheSg = mkApp (mkCId "TheSg") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "APl" -> GAPl
      Just (i,[]) | i == mkCId "ASg" -> GASg
      Just (i,[]) | i == mkCId "All" -> GAll
      Just (i,[]) | i == mkCId "Any" -> GAny
      Just (i,[]) | i == mkCId "AnyOther" -> GAnyOther
      Just (i,[]) | i == mkCId "ThePl" -> GThePl
      Just (i,[]) | i == mkCId "TheSg" -> GTheSg


      _ -> error ("no Determiner " ++ show t)

instance Gf GKind where
  gf GCapital = mkApp (mkCId "Capital") []
  gf GChangeOfControl = mkApp (mkCId "ChangeOfControl") []
  gf (GComplKind x1 x2) = mkApp (mkCId "ComplKind") [gf x1, gf x2]
  gf GDirectListing = mkApp (mkCId "DirectListing") []
  gf GDissolutionEvent = mkApp (mkCId "DissolutionEvent") []
  gf GEquityFinancing = mkApp (mkCId "EquityFinancing") []
  gf GEvent = mkApp (mkCId "Event") []
  gf GGeneralAssignment = mkApp (mkCId "GeneralAssignment") []
  gf GInitialPublicOffering = mkApp (mkCId "InitialPublicOffering") []
  gf (GKProperty x1 x2) = mkApp (mkCId "KProperty") [gf x1, gf x2]
  gf (GKWhetherOr x1 x2) = mkApp (mkCId "KWhetherOr") [gf x1, gf x2]
  gf GLiquidityEvent = mkApp (mkCId "LiquidityEvent") []
  gf GPreferredStock = mkApp (mkCId "PreferredStock") []
  gf (GSingleOrSeries x1) = mkApp (mkCId "SingleOrSeries") [gf x1]
  gf GTermination = mkApp (mkCId "Termination") []
  gf GTransaction = mkApp (mkCId "Transaction") []
  gf GValuation = mkApp (mkCId "Valuation") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "Capital" -> GCapital
      Just (i,[]) | i == mkCId "ChangeOfControl" -> GChangeOfControl
      Just (i,[x1,x2]) | i == mkCId "ComplKind" -> GComplKind (fg x1) (fg x2)
      Just (i,[]) | i == mkCId "DirectListing" -> GDirectListing
      Just (i,[]) | i == mkCId "DissolutionEvent" -> GDissolutionEvent
      Just (i,[]) | i == mkCId "EquityFinancing" -> GEquityFinancing
      Just (i,[]) | i == mkCId "Event" -> GEvent
      Just (i,[]) | i == mkCId "GeneralAssignment" -> GGeneralAssignment
      Just (i,[]) | i == mkCId "InitialPublicOffering" -> GInitialPublicOffering
      Just (i,[x1,x2]) | i == mkCId "KProperty" -> GKProperty (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "KWhetherOr" -> GKWhetherOr (fg x1) (fg x2)
      Just (i,[]) | i == mkCId "LiquidityEvent" -> GLiquidityEvent
      Just (i,[]) | i == mkCId "PreferredStock" -> GPreferredStock
      Just (i,[x1]) | i == mkCId "SingleOrSeries" -> GSingleOrSeries (fg x1)
      Just (i,[]) | i == mkCId "Termination" -> GTermination
      Just (i,[]) | i == mkCId "Transaction" -> GTransaction
      Just (i,[]) | i == mkCId "Valuation" -> GValuation


      _ -> error ("no Kind " ++ show t)

instance Gf GKind_Term where
  gf (GConjSlashTerm x1 x2) = mkApp (mkCId "ConjSlashTerm") [gf x1, gf x2]
  gf GDissolution = mkApp (mkCId "Dissolution") []
  gf GLiquidation = mkApp (mkCId "Liquidation") []
  gf GWindingUp = mkApp (mkCId "WindingUp") []

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "ConjSlashTerm" -> GConjSlashTerm (fg x1) (fg x2)
      Just (i,[]) | i == mkCId "Dissolution" -> GDissolution
      Just (i,[]) | i == mkCId "Liquidation" -> GLiquidation
      Just (i,[]) | i == mkCId "WindingUp" -> GWindingUp


      _ -> error ("no Kind_Term " ++ show t)

instance Gf GListAction where
  gf (GListAction [x1,x2]) = mkApp (mkCId "BaseAction") [gf x1, gf x2]
  gf (GListAction (x:xs)) = mkApp (mkCId "ConsAction") [gf x, gf (GListAction xs)]
  fg t =
    GListAction (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BaseAction" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsAction" -> fg x1 : fgs x2


      _ -> error ("no ListAction " ++ show t)

instance Gf GListAction_Dir where
  gf (GListAction_Dir [x1,x2]) = mkApp (mkCId "BaseAction_Dir") [gf x1, gf x2]
  gf (GListAction_Dir (x:xs)) = mkApp (mkCId "ConsAction_Dir") [gf x, gf (GListAction_Dir xs)]
  fg t =
    GListAction_Dir (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BaseAction_Dir" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsAction_Dir" -> fg x1 : fgs x2


      _ -> error ("no ListAction_Dir " ++ show t)

instance Gf GListAction_Dir_Indir where
  gf (GListAction_Dir_Indir [x1,x2]) = mkApp (mkCId "BaseAction_Dir_Indir") [gf x1, gf x2]
  gf (GListAction_Dir_Indir (x:xs)) = mkApp (mkCId "ConsAction_Dir_Indir") [gf x, gf (GListAction_Dir_Indir xs)]
  fg t =
    GListAction_Dir_Indir (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BaseAction_Dir_Indir" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsAction_Dir_Indir" -> fg x1 : fgs x2


      _ -> error ("no ListAction_Dir_Indir " ++ show t)

instance Gf GListAction_Indir where
  gf (GListAction_Indir [x1,x2]) = mkApp (mkCId "BaseAction_Indir") [gf x1, gf x2]
  gf (GListAction_Indir (x:xs)) = mkApp (mkCId "ConsAction_Indir") [gf x, gf (GListAction_Indir xs)]
  fg t =
    GListAction_Indir (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BaseAction_Indir" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsAction_Indir" -> fg x1 : fgs x2


      _ -> error ("no ListAction_Indir " ++ show t)

instance Gf GListKind where
  gf (GListKind [x1,x2]) = mkApp (mkCId "BaseKind") [gf x1, gf x2]
  gf (GListKind (x:xs)) = mkApp (mkCId "ConsKind") [gf x, gf (GListKind xs)]
  fg t =
    GListKind (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BaseKind" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsKind" -> fg x1 : fgs x2


      _ -> error ("no ListKind " ++ show t)

instance Gf GListKind_Term where
  gf (GListKind_Term [x1,x2]) = mkApp (mkCId "BaseKind_Term") [gf x1, gf x2]
  gf (GListKind_Term (x:xs)) = mkApp (mkCId "ConsKind_Term") [gf x, gf (GListKind_Term xs)]
  fg t =
    GListKind_Term (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BaseKind_Term" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsKind_Term" -> fg x1 : fgs x2


      _ -> error ("no ListKind_Term " ++ show t)

instance Gf GListProperty where
  gf (GListProperty [x1,x2]) = mkApp (mkCId "BaseProperty") [gf x1, gf x2]
  gf (GListProperty (x:xs)) = mkApp (mkCId "ConsProperty") [gf x, gf (GListProperty xs)]
  fg t =
    GListProperty (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BaseProperty" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsProperty" -> fg x1 : fgs x2


      _ -> error ("no ListProperty " ++ show t)

instance Gf GListTerm where
  gf (GListTerm [x1,x2]) = mkApp (mkCId "BaseTerm") [gf x1, gf x2]
  gf (GListTerm (x:xs)) = mkApp (mkCId "ConsTerm") [gf x, gf (GListTerm xs)]
  fg t =
    GListTerm (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BaseTerm" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsTerm" -> fg x1 : fgs x2


      _ -> error ("no ListTerm " ++ show t)

instance Gf GMove where
  gf (GMAction x1 x2 x3) = mkApp (mkCId "MAction") [gf x1, gf x2, gf x3]
  gf (GMDefProp x1 x2) = mkApp (mkCId "MDefProp") [gf x1, gf x2]
  gf (GMDefTerm x1 x2) = mkApp (mkCId "MDefTerm") [gf x1, gf x2]
  gf GMNo = mkApp (mkCId "MNo") []
  gf (GMQuery x1) = mkApp (mkCId "MQuery") [gf x1]
  gf GMYes = mkApp (mkCId "MYes") []

  fg t =
    case unApp t of
      Just (i,[x1,x2,x3]) | i == mkCId "MAction" -> GMAction (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "MDefProp" -> GMDefProp (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "MDefTerm" -> GMDefTerm (fg x1) (fg x2)
      Just (i,[]) | i == mkCId "MNo" -> GMNo
      Just (i,[x1]) | i == mkCId "MQuery" -> GMQuery (fg x1)
      Just (i,[]) | i == mkCId "MYes" -> GMYes


      _ -> error ("no Move " ++ show t)

instance Gf GProperty where
  gf GBonaFide = mkApp (mkCId "BonaFide") []
  gf (GConjProperty x1 x2) = mkApp (mkCId "ConjProperty") [gf x1, gf x2]
  gf GFixed = mkApp (mkCId "Fixed") []
  gf (GForBenefit x1) = mkApp (mkCId "ForBenefit") [gf x1]
  gf (GPNeg x1) = mkApp (mkCId "PNeg") [gf x1]
  gf GPostMoney = mkApp (mkCId "PostMoney") []
  gf GPreMoney = mkApp (mkCId "PreMoney") []
  gf GVoluntary = mkApp (mkCId "Voluntary") []
  gf (GWithPurpose x1) = mkApp (mkCId "WithPurpose") [gf x1]

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "BonaFide" -> GBonaFide
      Just (i,[x1,x2]) | i == mkCId "ConjProperty" -> GConjProperty (fg x1) (fg x2)
      Just (i,[]) | i == mkCId "Fixed" -> GFixed
      Just (i,[x1]) | i == mkCId "ForBenefit" -> GForBenefit (fg x1)
      Just (i,[x1]) | i == mkCId "PNeg" -> GPNeg (fg x1)
      Just (i,[]) | i == mkCId "PostMoney" -> GPostMoney
      Just (i,[]) | i == mkCId "PreMoney" -> GPreMoney
      Just (i,[]) | i == mkCId "Voluntary" -> GVoluntary
      Just (i,[x1]) | i == mkCId "WithPurpose" -> GWithPurpose (fg x1)


      _ -> error ("no Property " ++ show t)

instance Gf GQuery where
  gf (GQWhetherProp x1 x2) = mkApp (mkCId "QWhetherProp") [gf x1, gf x2]
  gf (GQWhetherTerm x1 x2) = mkApp (mkCId "QWhetherTerm") [gf x1, gf x2]
  gf (GQWhichProp x1 x2) = mkApp (mkCId "QWhichProp") [gf x1, gf x2]
  gf (GQWhichTerm x1 x2) = mkApp (mkCId "QWhichTerm") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "QWhetherProp" -> GQWhetherProp (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "QWhetherTerm" -> GQWhetherTerm (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "QWhichProp" -> GQWhichProp (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "QWhichTerm" -> GQWhichTerm (fg x1) (fg x2)


      _ -> error ("no Query " ++ show t)

instance Gf GTemporality where
  gf GTFuture = mkApp (mkCId "TFuture") []
  gf GTPast = mkApp (mkCId "TPast") []
  gf GTPresent = mkApp (mkCId "TPresent") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "TFuture" -> GTFuture
      Just (i,[]) | i == mkCId "TPast" -> GTPast
      Just (i,[]) | i == mkCId "TPresent" -> GTPresent


      _ -> error ("no Temporality " ++ show t)

instance Gf GTerm where
  gf GCompany = mkApp (mkCId "Company") []
  gf (GConjTerm x1 x2) = mkApp (mkCId "ConjTerm") [gf x1, gf x2]
  gf (GCreditors x1) = mkApp (mkCId "Creditors") [gf x1]
  gf (GRelDir x1 x2 x3 x4) = mkApp (mkCId "RelDir") [gf x1, gf x2, gf x3, gf x4]
  gf (GRelIndir x1 x2 x3 x4) = mkApp (mkCId "RelIndir") [gf x1, gf x2, gf x3, gf x4]
  gf (GTDet x1 x2) = mkApp (mkCId "TDet") [gf x1, gf x2]
  gf (GTExcluding x1 x2 x3) = mkApp (mkCId "TExcluding") [gf x1, gf x2, gf x3]
  gf (GTIncluding x1 x2 x3) = mkApp (mkCId "TIncluding") [gf x1, gf x2, gf x3]

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "Company" -> GCompany
      Just (i,[x1,x2]) | i == mkCId "ConjTerm" -> GConjTerm (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "Creditors" -> GCreditors (fg x1)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "RelDir" -> GRelDir (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "RelIndir" -> GRelIndir (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2]) | i == mkCId "TDet" -> GTDet (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "TExcluding" -> GTExcluding (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3]) | i == mkCId "TIncluding" -> GTIncluding (fg x1) (fg x2) (fg x3)


      _ -> error ("no Term " ++ show t)
