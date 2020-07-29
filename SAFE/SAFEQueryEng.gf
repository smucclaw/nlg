concrete SAFEQueryEng of SAFEQuery = QueryEng **
  open
  Prelude,
  (R=ResEng),
  (E=ExtendEng),
  (C=ConjunctionEng),
  SyntaxEng,
  ParadigmsEng,
  NounEng,
  VerbEng,
  AdjectiveEng in {

  lincat
    Action = LinAction ; -- Negations affect more than standard RGL negation does
    'Action/Dir' = SlashDir ;
    'Action/Indir' = SlashIndir ;
    'Action/Dir/Indir' = SlashDirIndir ;
    [Action] = ListLinAction ;
    ['Action/Dir'] = ListSlashDir ;
    ['Action/Indir'] = ListSlashIndir ;
    ['Action/Dir/Indir'] = ListSlashDirIndir ;

    Temporality = Tense ;
    Polarity = Pol ;

  linref
    Action = linAction ;

  lin
    -------------
    -- Actions --
    -------------
    -- Direct object
    Raise = mkDir raise_V2 ;
    Issue = mkDir issue_V2 ;
    Sell  = mkDir sell_V2 ;

    -- Indirect object
    IssueAt = mkDirIndir issue_at_V3 whether_at_Prep ;
    SellAt = mkDirIndir sell_at_V3 whether_at_Prep ;

    -- Complements
    -- : 'Action/Dir' -> Term -> Action ;
    AComplDir = complDir ;
    -- : 'Action/Indir' -> Term -> Action ;
    AComplIndir = complIndir ;
    -- : 'Action/Dir/Indir' -> Term -> 'Action/Indir' ; -- sell stock (at fixed valuation)
    ASlashDir = slashDir ;
    -- : 'Action/Dir/Indir' -> Term -> 'Action/Dir' ;   -- sell (stock) at fixed valuation
    ASlashIndir = slashIndir ;

    -- Negations
    -- : Action -> Action ;        -- doesn't sell X / doesn't sell X and Y
    ANeg action = action ** {
      s = \\t,p => case p of {
        --R.CNeg _ => action.s ! t ! R.CPos ; -- double negation = positive
        _ => action.s ! t ! negativePol.p
        } ;
      gerund = table {
        --R.Neg => action.gerund ! R.Pos ; -- double negation = positive
        _ => action.gerund ! R.Neg
        }
      } ;
    -- : 'Action/Dir' -> [Term] -> Action ; -- sells neither X, Y nor Z
    AComplNoneDir v2 obj =
      let none_of : NP = mkNP neither7nor_DConj obj ;
       in complDir v2 none_of ;

    --  AComplNoneIndir : 'Action/Indir' -> [Term] -> Action ; -- sells (X) neither to B nor to B


    -- Conjunctions
    BaseAction a1 a2 = {
      s = \\t,p => E.BaseVPS (a1.s ! t ! p) (a2.s ! t ! p) ; -- doesn't sell X and doesn't issue Y
      gerund = \\p => mkListAdv (a1.gerund ! p) (a2.gerund ! p) -- not selling X and not issuing Y
      } ;
    ConsAction a as = {
      s = \\t,p => E.ConsVPS (a.s ! t ! p) (as.s ! t ! p) ;
      gerund = \\p => mkListAdv (a.gerund ! p) (as.gerund ! p)
      } ;
    ConjAction co as = {
      s = \\t,p =>
        E.ConjVPS co (as.s ! t ! p) ;
      gerund = \\p =>
        SyntaxEng.mkAdv co (as.gerund ! p)
      } ;

    'BaseAction/Dir' a1 a2 =
      let a1' : LinAction = complDir a1 emptyTerm ;
          a2' : LinAction = complDir a2 emptyTerm ;
      in BaseAction a1' a2' ** {
        dir = a1.dir ; -- : PrepPol
        indir = \\p => emptyAdv ; -- the existing indir has been incorporated in a1' and a2'
      } ;
    'ConsAction/Dir' a as =
      let a' : LinAction = complDir a emptyTerm ;
      in ConsAction a' <as:ListLinAction> ** {
        dir = as.dir ; -- : PrepPol
        indir = \\p => emptyAdv
        } ;
    ConjSlashDir co as = ConjAction co as ** {
      dir = as.dir ;
      indir = as.indir
      } ;

    'BaseAction/Indir' a1 a2 =
      let a1' : LinAction = complIndir a1 emptyTerm ;
          a2' : LinAction = complIndir a2 emptyTerm ;
      in BaseAction a1' a2' ** {
        indir = a1.indir ; -- : PrepPol
       dir = \\p => emptyAdv ; -- the existing dir has been incorporated in a1' and a2'
      } ;
    'ConsAction/Indir' a as =
      let a' : LinAction = complIndir a emptyTerm ;
      in ConsAction a' <as:ListLinAction> ** {
        indir = as.indir ; -- : PrepPol
        dir = \\p => emptyAdv
        } ;

    ConjSlashIndir co as = ConjAction co as ** {
      dir = as.dir ;
      indir = as.indir
      } ;

    'BaseAction/Dir/Indir' a1 a2 = BaseAction a1 a2 ** {
      dir = a2.dir ;
      indir = a2.indir
      } ;
    'ConsAction/Dir/Indir' a as = ConsAction a as ** {
      dir = as.dir ;
      indir = as.indir
      } ;
    ConjSlashDirIndir co as = ConjAction co as ** {
      dir = as.dir ;
      indir = as.indir
      } ;

    -- : Term -> Action -> Move ; -- the company raises capital
    MAction temp t a = mkText (mkUtt (cl temp PPositive t a)) fullStopPunct ;

    TPresent = presentTense ;
    TPast = pastTense ;
    PPositive = positivePol ;
    PNegative = negativePol ;

  oper
    LinAction : Type = {
      s : R.Tense => R.CPolarity => E.VPS ;
      gerund : R.Polarity => Adv ;
      } ;

    ListLinAction : Type = {
      s : R.Tense => R.CPolarity => E.ListVPS ;
      gerund : R.Polarity => [Adv] ;
      } ;

    linAction : LinAction -> Str = \l ->
      (mkUtt (cl presentTense positivePol emptyTerm l)).s ;

    mkVPS : R.Tense -> R.CPolarity -> VP -> E.VPS = \t,p ->
      let tense : Tense = lin Tense {s=[] ; t=t} ;
          pol : Pol = lin Pol {s=[] ; p=p} ;
       in E.MkVPS (mkTemp tense simultaneousAnt) pol ;

    emptyTerm : LinTerm = emptyNP ;
    ----------------------
    -- Slash categories --
    ----------------------

    mkGerS : V2 -> LinAction = \v2 -> {
      s = \\t,p => mkVPS t p (mkVP <v2:V2> emptyNP) ;
      gerund =
        let posAdv : Adv = E.GerundAdv (mkVP <v2:V2> emptyNP) ;
            negAdv : Adv = posAdv ** {s = "not" ++ posAdv.s}
        in table {
          R.Pos => posAdv ;
          R.Neg => negAdv }
      } ;

    -- /Dir
    SlashDir : Type = LinAction ** {
      indir : R.CPolarity => Adv ; -- at fixed valuation / whether at fv nor without fv
      dir : PrepPol
      } ;
    mkDir : V2 -> SlashDir = \v2 -> mkGerS v2 ** {
      dir = prepPol v2.c2 ;
      indir = \\_ => emptyAdv ;
      } ;
    slashDir : SlashDirIndir -> LinTerm -> SlashIndir = \vps,do -> vps ** {
      dir = applyPrepPol vps.dir do
      } ;
    complDir : SlashDir -> LinTerm -> LinAction = \vps,do -> vps ** {
      s = \\t,p => complS (vps.s ! t ! p)
                          (applyPrepPol vps.dir do ! p)
                          (vps.indir ! p) ;
      gerund = \\p => complGer (vps.gerund ! p)
                            (applyPrepPol vps.dir do ! pol2cpol p)
                            (vps.indir ! pol2cpol p) ;
      } ;

    -- /Indir
    SlashIndir : Type = LinAction ** {
      dir : R.CPolarity => Adv ; -- (Acme will/won't sell) some/any stock
      indir : PrepPol ;
      } ;
    mkIndir : V2 -> SlashIndir = \v2 -> mkGerS v2 ** {
      dir = \\_ => emptyAdv ;
      indir = prepPol v2.c2 ;
      } ;
    slashIndir : SlashDirIndir -> LinTerm -> SlashDir = \vps,io -> vps ** {
      indir = applyPrepPol vps.indir io
      } ;
    complIndir : SlashIndir -> LinTerm -> LinAction = \vps,io -> vps ** {
      s = \\t,p => complS (vps.s ! t ! p)
                          (vps.dir ! p)
                          (applyPrepPol vps.indir io ! p) ;
      gerund = \\p => complGer (vps.gerund ! p)
                            (vps.dir ! pol2cpol p)
                            (applyPrepPol vps.indir io ! pol2cpol p)
      } ;


    -- /Dir/Indir
    SlashDirIndir : Type = LinAction ** {
      dir,
      indir : PrepPol ;
      } ;
    mkDirIndir = overload {
      mkDirIndir : V3 -> SlashDirIndir = \v3 -> mkGerS v3 ** {
        dir = prepPol v3.c2 ;
        indir = prepPol v3.c3
        } ;
      mkDirIndir : V3 -> PrepPol -> SlashDirIndir = \v3,indir -> mkGerS v3 ** {
        indir = indir ;
        dir = prepPol v3.c2
        }
      } ;
    -- PrepPol is more powerful than Prep: prepared for multilayer negations
    PrepPol : Type = R.CPolarity => PrepPlus ;
    PrepPlus : Type = {  -- Positive version  / Negative version
      s : Str ;      -- at (fixed valuation) / whether at (fixed valuation)
      post : Str ;   -- ∅                    / or without
      redupl : Bool  -- False                / True       (fixed valuation)
      } ;

    prepPol = overload {
      prepPol : Str -> PrepPol = \p -> \\pol => {
        s = p ;
        post = [] ;
        redupl = False
        } ;
      prepPol : (p,n : PrepPlus) -> PrepPol = \pos,neg -> table {
        R.CPos   => pos ;
        R.CNeg _ => neg
        }
      } ;

    prepPlus : (s,post : Str) -> (redupl : Bool) -> PrepPlus = \s,post,r -> {
      s = s ;
      post = post ;
      redupl = r
      } ;

    applyPrepPol : PrepPol -> LinTerm -> (R.CPolarity=>Adv) = \pp,term -> \\pol =>
      let np : NP = term ; -- ! cpol2pol pol ;
          npacc : Str = np.s ! R.NPAcc ;
          prep : PrepPlus = pp ! pol
      in lin Adv {
        s = prep.s ++ npacc ++ prep.post ++ case prep.redupl of {
                                                True => npacc ;
                                                False => [] }
      } ;

    -- helpers for complDir and complIndir
    complS : E.VPS -> Adv -> Adv -> E.VPS = \vps,dir,indir -> lin VPS {
      s = \\a => vps.s ! a ++ dir.s ++ indir.s
      } ;
    complGer : (a,b,c : Adv) -> Adv = \ger,indir,dir -> lin Adv {
      s = ger.s ++ dir.s ++ indir.s
      } ;

    -------------------
    -- List versions --
    -------------------
    ListSlashDir : Type = ListLinAction ** {
      indir : R.CPolarity => Adv ; -- at fixed valuation / whether at fv nor without fv
      dir : PrepPol ;
      } ;

    ListSlashIndir : Type = ListLinAction ** {
      dir : R.CPolarity => Adv ; -- (Acme will/won't sell) some/any stock
      indir : PrepPol ;
      } ;

    ListSlashDirIndir : Type = ListLinAction ** {
      dir,
      indir : PrepPol ;
      } ;

    ---------------------
    -- Generic helpers --
    ---------------------
    cl : Tense -> Polarity -> LinTerm -> LinAction -> S = \t,p,subj,pred ->
      let s : S = E.PredVPS (np subj) (pred.s ! t.t ! p.p)
       in s ** {s = s.s ++ t.s ++ p.s} ;
    -- This is silly, but I need to do it this way, because instead of VP, which is variable in
    -- tense and polarity, I'm storing /fully formed VPS/s in a table with R.Tense and R.CPolarity as LHS.
    -- (Why do I store VPS instead of VP? To be able to coordinate them.)
    -- When an abstract syntax value like TPresent or PPositive is used to choose the correct VPS,
    -- I need to use the s fields of those values, so that every argument contributes to the linearization.
    -- See https://inariksit.github.io/gf/2018/08/28/gf-gotchas.html#metavariables-or-those-question-marks-that-appear-when-parsing

    gerund : LinAction -> R.Polarity=>NP = \pred -> \\pol =>
      let s : Str = (pred.gerund ! pol).s in mkNP (mkN s s s s) ;

    cpol2pol : R.CPolarity -> R.Polarity = \p -> case p of {
      R.CPos => R.Pos ;
      R.CNeg _ => R.Neg
      } ;

    pol2cpol : R.Polarity -> R.CPolarity = \p -> case p of {
      R.Pos => R.CPos ;
      R.Neg => R.CNeg True
      } ;

  lin

    ----------------
    -- Properties --
    ----------------
    Fixed = prop "fixed" ;
    PreMoney = prop "pre-money" ;
    PostMoney = prop "post-money" ;
    BonaFide = prop "bona fide" ;
    Voluntary = prop "voluntary" "involuntary" ;

    -- : Term -> Property ; -- for the benefit of the Company's creditors
    ForBenefit t =
      let for_b : Adv = adv for_Prep (mkNP the_Det (mkCN benefit_N2 (np t))) ;
          not_for_b : Adv = for_b ** ss2 "not" for_b.s ;
      in table {
        R.Pos => adv2ap for_b ;
        R.Neg => adv2ap not_for_b
      } ;

    -- : Action -> Property ; -- with the purpose of raising capital
    WithPurpose action =
      let purpose_of_NP : NP = mkNP the_Det (mkCN purpose_N2 (gerund action ! R.Pos)) ;
       in table {
         R.Pos => adv2ap (adv with_Prep purpose_of_NP) ;
         R.Neg => adv2ap (adv without_Prep purpose_of_NP)
      } ;


    -----------
    -- Kinds --
    -----------
    Event = kind "event" ;
    Capital = kind "capital" ** {k = Mass} ;

    DissolutionEvent = adjkind "dissolution" "event" ;
    Termination = ofkind "termination" "operations" ;
    GeneralAssignment = adjkind "general" "assignment" ;

    LiquidityEvent = adjkind "liquidity" "event" | kind "liquidity" ;
    ChangeOfControl = ofkind "change" "control" ;
    DirectListing = adjkind "direct" "listing" ;
    InitialPublicOffering = linkind (mkCN (mkA "initial") (adjkind "public" "offering").cn) ;

    EquityFinancing = adjkind "equity" "financing" ** {k = Mass} ;
    Transaction = kind "transaction" ;
    PreferredStock = adjkind "preferred" "stock" ** {k = Mass};
    Valuation = kind "valuation" ;

  lincat
    'Kind/Term' = CN ;
    'ListKind/Term' = C.ListCN ;

  lin
    Liquidation = mkCN (mkN "liquidation") ;
    Dissolution = mkCN (mkN "dissolution") ;
    WindingUp = mkCN (mkN "winding up" "windings up") ;

    -- Complement goes to cn field, not to adv field.
    ComplKind cn t = linkind (mkCN cn (adv part_Prep (np t))) ;

    -- : [Property] -> Kind -> Kind
    KWhetherOr props kind =
      let prop : Adv = ap2adv (mkAP whether_or_Conj (props ! R.Pos)) ;
      in kind ** {
        adv = cc2 kind.adv prop } ;

    'BaseKind/Term' = C.BaseCN ;
    'ConsKind/Term' = C.ConsCN ;
    ConjSlashTerm = C.ConjCN ;

    -----------
    -- Terms --
    -----------
    -- : Term
    Company = --\\_ =>
      mkNP theSg_Det (mkN "Company") ;

    -- : Term -> Term ;
    Creditors t = --\\_ =>   -- the company's creditors
      mkNP (mkDet (ExtendEng.GenNP (np t)) pluralNum) creditor_N ;

    -- : Determiner -> Kind -> Term -> Term ;
    TExcluding the valuation t =
      let exclAdv : Adv = parenss (adv excluding_Prep (np t)) ; -- The adv "excluding post-money"
          valuation_excl : Kind = valuation ** {
            cn = AdvCN valuation.cn exclAdv  -- first layer: "valuation excluding post-money"
            } ; -- Potential postmodifier is in valuation's adv field
      in term the valuation_excl ;

    -- : Determiner -> Kind -> Term -> Term ;
    TIncluding the valuation t = -- fixed valuation, including a pre-money or post-money valuation
      let inclAdv : Adv = adv including_Prep (np t) ; -- The adv "including pre-money"
          valuation_incl : Kind = valuation ** {
            cn = ExtAdvCN valuation.cn inclAdv  -- first layer: "valuation including pre-money"
            } ; -- Potential postmodifier is in valuation's adv field
      in term the valuation_incl ;

    -- : Term -> Term -> Action -> Term ;
    UnderWhich contract company sells_stock =
      let under : Str = "pursuant to" ;
          dummyRS : RS = mkRS (mkRCl (mkCl (mkN "dummy"))) ; -- to get all fields in RS and not touch RGL internals. TODO: eventually add this construction to Extend.
          s : S = cl presentTense positivePol company sells_stock ;
          rs : RS = dummyRS ** {s = \\agr => under ++ "which" ++ s.s} ;
       in mkNP contract rs ;

    AnyOther = any_other_Det ;
    Series = series_Det ;

  oper
    -------------
    -- Lexicon --
    -------------
    any_other_Det : LinDet = \\_ => a_Det ** {s = "any other"} ;
    series_Det : LinDet = \\_ => aPl_Det ** {s = "series of"} ;

    raise_V2 : V2 = mkV2 (mkV "raise") ;
    sell_V2 : V2 = mkV2 (mkV "sell") ;
    issue_V2 : V2 = mkV2 (mkV "issue") ;
    sell_at_V3 : V3 = mkV3 (mkV "sell") noPrep at_Prep ;
    issue_at_V3 : V3 = mkV3 (mkV "issue") noPrep at_Prep ;

    benefit_N2 : N2 = mkN2 (mkN "benefit") ;
    purpose_N2 : N2 = mkN2 (mkN ("purpose"|"principal purpose")) ;
    series_N2 : N2 = mkN2 (mkN "series" "series") ;
    creditor_N : N = mkN "creditor" ;

    whether_or_Conj : Conj = or_Conj ** {s1 = ", whether"} ;

    whether_at_Prep : PrepPol =
     prepPol
      {s = "at" ; post = [] ; redupl = False}
      {s = ", whether at" ; post = "or without" ; redupl = True} ;

    at_Prep : Prep = mkPrep "at" ;
    excluding_Prep : Prep = mkPrep "excluding" ;
    including_Prep : Prep =  -- endComma: disappears in front of other punctuation
      mkPrep ("including" ++ strOpt ("but not limited to" ++ Prelude.endComma)) ;


    ----------
    -- Misc --
    ----------
    linkind : CN -> LinKind = \cn -> {cn = cn ; adv = emptyAdv ; k = Count} ;

    adv : Prep -> NP -> Adv = SyntaxEng.mkAdv ; -- shorthand: mkAdv is imported from two modules, so it has to be qualified
    prop = overload {
      prop : Str -> Str -> LinProp = \pos,neg -> table {
        R.Pos => mkAP (mkA pos) ;
        R.Neg => mkAP (mkA neg)
        } ;
      prop : Str -> LinProp = \pos -> table {
        R.Pos => mkAP (mkA pos) ;
        R.Neg => mkAP (mkA ("not" ++ pos))
        }
      } ;
    kind : Str -> LinKind = \n -> linkind (mkCN (mkN n)) ;
    adjkind : Str -> Str -> LinKind =
      \a,n -> linkind (mkCN (mkAP (mkA a)) (mkN n)) ;
    ofkind : Str -> Str -> LinKind =
      \n,p -> linkind (mkCN (mkN n) (adv part_Prep (mkNP (mkN p)))) ;

    ExtAdvCN : CN -> Adv -> CN = \cn,ad -> cn ** {   -- RGL fun AdvCN doesn't put comma
      s = \\n,c => cn.s ! n ! c ++ "," ++ ad.s
      } ;

 }
