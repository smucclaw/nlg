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

    -- Following categories on the RHS (Text, QS etc.) come from SyntaxEng
    Move = Text ;
    Query = QS ;
    Property = AP ;  -- Simplification: later use https://github.com/GrammaticalFramework/gf-contrib/blob/master/YAQL/YAQLFunctor.gf#L19
    [Property] = ListAP ;


  linref
    -- To make discontinuous categories show properly in the shell
    Kind = \x -> (mkUtt (merge x)).s ;
    Term = \x -> (mkUtt (np x)).s ;

  lin
    -- : Query -> Move ;  -- Coercion function: Query to start category Move
    MQuery q = mkText (mkUtt q) questMarkPunct ;

    -- : Kind -> Property -> Query ;    -- which events are dissolution events
    QWhichProp kind prop = mkQS (mkQCl (mkIP whichPl_IDet (merge kind)) prop) ;

    -- : Kind -> Term -> Query ;      -- which events are dissolution events
    QWhichTerm kind term = mkQS (mkQCl (mkIP whichPl_IDet (merge kind)) (np term)) ;

    -- : Term -> Property -> Query ;    -- is change of control voluntary
    QWhetherProp term prop = mkQS (mkQCl (mkCl (np term) prop)) ;

    -- : Term -> Term -> Query ;    -- is change of control a liquidity event
    QWhetherTerm term1 term2 = mkQS (mkQCl (mkCl (np term1) (np term2))) ;

    MYes = yes_Utt ;
    MNo = no_Utt ;

    -- : Kind -> Property -> Move ;     -- liquidity event is voluntary
    MDefProp kind prop = mkText (mkUtt (mkCl (defTerm kind) prop)) fullStopPunct ;

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
    KProperty prop kind = case prop.isPre of {
      True => kind ** { -- voluntary termination
        cn = mkCN prop kind.cn
        } ;
      False => kind ** { -- termination for the benefit of the Company
        adv = cc2 kind.adv (ap2adv prop)
        }
      } ;

    --PNot : Property -> Property ;

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
    -- See the two instances of mkListAP https://www.grammaticalframework.org/lib/doc/synopsis/index.html#ListAP
    BaseProperty = mkListAP ; -- : AP -> AP -> ListAP
    ConsProperty = mkListAP ; -- : AP -> ListAP -> ListAP
    ConjProperty co = mkAP (co ! Pos) ;     -- : Conj -> ListAP -> AP

    -- See the two instances of mkListNP https://www.grammaticalframework.org/lib/doc/synopsis/index.html#ListNP
    -- Unlike in {Base,Cons}Property, now we transform t1 and t2 into NPs.
    -- (Actually their type is already NP, but if this changes later,
    -- we need to change only the definition of 'np', only in one place.)
    BaseTerm t1 t2 = \\pol =>
      mkListNP (t1 ! pol) (t2 ! pol) ; -- : NP -> NP -> ListNP
    ConsTerm t1 ts = \\pol =>
      mkListNP (t1 ! pol) (ts ! pol) ; -- : NP -> ListNP -> ListNP
    ConjTerm co ts = \\pol =>
      let conj : Conj = case pol of {
            Pos => co ! Pos ;
            Neg => or_Conj } ; -- neither-nor only for verbs, negation of and and or is or
      in  mkNP conj (ts ! pol) ;     -- : Conj -> ListNP -> NP ;
  -----------------------------------------------------------------
  param
    KType = Mass | Count | Plural ;

  oper
    DetLite : Type = {s : Str ;  n : ParamX.Number} ;

    LinKind : Type = {
      cn : CN ;
      adv : Adv ;
      k : KType
      } ;
    LinTerm : Type = ParamX.Polarity => NP ;
    LinConj : Type = ParamX.Polarity => Conj ;
    LinDet : Type = KType => ParamX.Polarity => DetLite ;
    ListLinTerm : Type = ParamX.Polarity => ListNP ;

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

    emptyAdv : Adv = mkAdv [] ;
    emptyAP : AP = <mkAP (mkA []) : AP> ** {s = \\_ => []} ;
    emptyNP : NP = <mkNP (mkN []) : NP> ** {s = \\_ => []} ;

    ap2adv : AP -> Adv = \ap -> lin Adv (mkUtt ap) ;  -- RGL has no AP->Adv fun
    adv2ap : Adv -> AP = AdjectiveEng.AdvAP emptyAP ; -- RGL has no Adv->AP fun

    -- Combine Determiner and Kind into a Term
    term : LinDet -> LinKind -> LinTerm = \dets,kind ->
      \\pol => detCNLite (dets ! kind.k ! pol) (merge kind) ;

    defTerm : LinKind -> NP = \k -> mkNP (merge k) ;

    -- Merge the discontinuous Kind into a single CN
    merge : LinKind -> CN = \kind -> mkCN kind.cn kind.adv ;

    -- Default use for most NPs: pick the positive version (e.g. "some car", not "any car")
    np : LinTerm -> NP = \lt -> lt ! Pos ;

    -- np2cn : NP -> CN = \np -> let s : Str = (mkUtt np).s in mkCN (mkN s s s s) ;

    -- copied from RGL to work with DetLite
    detCNLite : DetLite -> CN -> NP = \det,cn -> lin NP {
      s = \\c => det.s ++ cn.s ! det.n ! R.npcase2case c ;
      a = R.agrgP3 det.n cn.g
      } ;
}
