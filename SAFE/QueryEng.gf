concrete QueryEng of Query = open
  Prelude,
  AdjectiveEng,
  SyntaxEng,
  ParamX,
  (R=ResEng),
  ParadigmsEng in {

  lincat
    -- LinKind and LinTerm are defined later in this file
    Kind = LinKind ;
    Term = LinTerm ;
    Determiner = LinDet ;
    Conjunction = LinConj ;
    [Term] = ListLinTerm ;
    Property = LinProp ;
    [Property] = ListLinProp ;

    -- Following categories on the RHS (Text, QS etc.) come from SyntaxEng
    Move = Text ;
    Query = QS ;


  linref
    -- To make discontinuous categories show properly in the shell
    Kind = \x -> (mkUtt (merge x)).s ;
    Term = \x -> (mkUtt (np x)).s ;

  lin
    -- : Query -> Move ;  -- Coercion function: Query to start category Move
    MQuery q = mkText (mkUtt q) questMarkPunct ;

    -- : Kind -> Property -> Query ;    -- which events are dissolution events
    QWhichProp kind prop = mkQS (mkQCl (mkIP whichPl_IDet (merge kind)) (ap prop)) ;

    -- : Kind -> Term -> Query ;      -- which events are dissolution events
    QWhichTerm kind term = mkQS (mkQCl (mkIP whichPl_IDet (merge kind)) (np term)) ;

    -- : Term -> Property -> Query ;    -- is change of control voluntary
    QWhetherProp term prop = mkQS (mkQCl (mkCl (np term) (ap prop))) ;

    -- : Term -> Term -> Query ;    -- is change of control a liquidity event
    QWhetherTerm term1 term2 = mkQS (mkQCl (mkCl (np term1) (np term2))) ;

    MYes = yes_Utt ;
    MNo = no_Utt ;

    -- : Kind -> Property -> Move ;     -- liquidity event is voluntary
    MDefProp kind prop = mkText (mkUtt (mkCl (defTerm kind) (ap prop))) fullStopPunct ;

    -- : Kind -> Term -> Move ;         -- liquidity event means A, B or C
    MDefTerm kind term = mkText (mkUtt (mkCl (defTerm kind) (mkV2 (mkV "mean")) (np term))) fullStopPunct ;

    -- Determiners
    ASg = table {
      Mass => \\_ => emptyDet ;
      Count => \\_ => aSg_Det ;
      Plural => \\_ => aPl_Det
      } ;
    APl = table {
      Mass => \\_ => emptyDet ;
      Count => \\_ => aPl_Det ;
      Plural => \\_ => aPl_Det
      } ;
    TheSg = table {
      Mass => \\_ => theSg_Det ;
      Count => \\_ => theSg_Det ;
      Plural => \\_ => thePl_Det
      } ;
    ThePl = table {
      Mass => \\_ => thePl_Det ;
      Count => \\_ => thePl_Det ;
      Plural => \\_ => thePl_Det
      } ;
    Any = table {
      Mass => \\_ => anySg_Det ;
      Count => \\_ => anySg_Det ;
      Plural => \\_ => anyPl_Det
      } ;
    All = table {
      Mass => table {Pos => allSg_Det ; Neg => anySg_Det} ;
      Count => table {Pos => all_Det ; Neg => anySg_Det} ;
      Plural => table {Pos => all_Det ; Neg => anyPl_Det}
      } ;

    -- Kinds, Terms and Properties
    -- : Determiner -> Kind -> Term
    TDet = term ; -- using our oper 'term', defined at the end of file

    -- : Property -> Kind -> Kind ;
    KProperty props kind = let prop : AP = ap props in
      case prop.isPre of {
        True => kind ** { -- voluntary termination
          cn = mkCN prop kind.cn
          } ;
        False => kind ** { -- termination for the benefit of the Company
          adv = cc2 kind.adv (ap2adv prop)
          }
      } ;

    -- : Property -> Property ;
    PNot prop = \\_ => prop ! Neg ;

    -- Conjunctions
    And = table {
      Pos => and_Conj ;
      Neg => neither7nor_DConj
      } ;
    Or = table {
      Pos => or_Conj ;
      Neg => neither7nor_DConj
      } ;

    -- Lists
    -- The opers base and cons are defined later in this file.
    -- The instances of mkListAP and mkListNP can be found at
    --   https://www.grammaticalframework.org/lib/doc/synopsis/index.html#ListAP
    --   https://www.grammaticalframework.org/lib/doc/synopsis/index.html#ListNP
    BaseProperty = base AP ListAP mkListAP ; -- : AP -> AP -> ListAP
    ConsProperty = cons AP ListAP mkListAP ; -- : AP -> ListAP -> ListAP
    ConjProperty co ps = \\pol =>               -- : Conj -> ListAP -> AP
      mkAP (co ! Pos) (ps ! pol) ; -- conjunctions don't change here, because negations can be lexical.
                                   -- e.g. "involuntary and unjustified"

    BaseTerm = base NP ListNP mkListNP ; -- : NP -> NP -> ListNP
    ConsTerm = cons NP ListNP mkListNP ; -- : NP -> ListNP -> ListNP
    ConjTerm co ts = \\pol =>
      let conj : Conj = case pol of {
            Pos => co ! Pos ;
            Neg => or_Conj } ; -- neither-nor only for verbs, negation of and and or is or for now
      in  mkNP conj (ts ! pol) ;     -- : Conj -> ListNP -> NP ;

  -----------------------------------------------------------------

  param
    KType = Mass | Count | Plural ;

  oper
    --------------------
    -- Types for lincats

    DetLite : Type = {s : Str ;  n : ParamX.Number} ;

    LinKind : Type = {
      cn : CN ;
      adv : Adv ;
      k : KType
      } ;
    LinTerm : Type = ParamX.Polarity => NP ;
    LinConj : Type = ParamX.Polarity => Conj ;
    LinDet : Type = KType => ParamX.Polarity => DetLite ;
    LinProp : Type = ParamX.Polarity => AP ;  -- Simplification: later use https://github.com/GrammaticalFramework/gf-contrib/blob/master/YAQL/YAQLFunctor.gf#L19

    --------
    -- Lists
    ListLinTerm : Type = ParamX.Polarity => ListNP ;
    ListLinProp : Type = ParamX.Polarity => ListAP ;

    -- This is how you do polymorphism in GF.
    -- A bit unwieldy.

    base : (A : Type) -> (ListA : Type) ->
      (A -> A -> ListA) ->
      (a, b : ParamX.Polarity => A) ->
      ParamX.Polarity => ListA =
      \_,_,mkList,a,b -> \\pol => mkList (a ! pol) (b ! pol) ;

    cons : (A : Type) -> (ListA : Type) ->
      (A -> ListA -> ListA) ->
      (a : ParamX.Polarity => A) ->
      (as : ParamX.Polarity => ListA) ->
      ParamX.Polarity => ListA =
      \_,_,mkList,a,as -> \\pol => mkList (a ! pol) (as ! pol) ;


    ------------
    -- Conj, Det

    neither7nor_DConj : Conj = mkConj "neither" "nor" singular ;

    -- Resource Grammar Library doesn't have any_Det, so we make it ourselves.
    -- Determiners are supposed to be closed class, so the constructor isn't
    -- exported in the API. (Silly, if you ask me.)
    -- The options are: open a low-level module and use the hidden constructor, or do this hack.
    anySg_Det : Det = a_Det ** { -- Extend a_Det: keyword ** is record extension
      s = "any"                -- Keep other fields from a_Det, but replace s with "any"
      } ;
    anyPl_Det : Det = aPl_Det ** {
      s = "any"
      } ;
    all_Det : Det = aPl_Det ** {
      s = "all"
      } ;
    allSg_Det : Det = a_Det ** {
      s = "all"
      } ;
    emptyDet : Det = a_Det ** {
      s = []
      } ;

    ----------------
    -- Empty phrases

    emptyAdv : Adv = mkAdv [] ;
    emptyAP : AP = <mkAP (mkA []) : AP> ** {s = \\_ => []} ;
    emptyNP : NP = <mkNP (mkN []) : NP> ** {s = \\_ => []} ;

    ap2adv : AP -> Adv = \ap -> lin Adv (mkUtt ap) ;  -- RGL has no AP->Adv fun
    adv2ap : Adv -> AP = AdjectiveEng.AdvAP emptyAP ; -- RGL has no Adv->AP fun

    -------------------------------
    -- Other useful syntactic opers

    -- Combine Determiner and Kind into a Term
    term : LinDet -> LinKind -> LinTerm = \dets,kind ->
      \\pol => detCNLite (dets ! kind.k ! pol) (merge kind) ;

    defTerm : LinKind -> NP = \k -> mkNP (merge k) ;

    -- Merge the discontinuous Kind into a single CN
    merge : LinKind -> CN = \kind -> mkCN kind.cn kind.adv ;

    -- Default use for most NPs: pick the positive version (e.g. "some car", not "any car")
    np : LinTerm -> NP = \lt -> lt ! Pos ;

    -- Default use for most APs: pick the positive version (e.g. "voluntary", not "involuntary")
    ap : LinProp -> AP = \lp -> lp ! Pos ;

    -- np2cn : NP -> CN = \np -> let s : Str = (mkUtt np).s in mkCN (mkN s s s s) ;

    -- copied from RGL to work with DetLite
    detCNLite : DetLite -> CN -> NP = \det,cn -> lin NP {
      s = \\c => det.s ++ cn.s ! det.n ! R.npcase2case c ;
      a = R.agrgP3 det.n cn.g
      } ;
}
