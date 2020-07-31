abstract SAFEQuery = Query ** {
  flags startcat=Move ;

  cat
    Action ;
    'Action/Dir' ;
    'Action/Indir' ;
    'Action/Dir/Indir' ;
    [Action]{2} ;              -- sells stock to Acme and raises capital
    ListActionDir ;            -- sells today and issues (stock) at fixed valuation
    ListActionIndir ;          -- sells widgets and issues stock (at fixed valuation)
    ListActionDirIndir ;       -- sells and issues (stock) (at fixed valuation)

  fun
    -------------
    -- Actions --
    -------------
    -- Direct object
    Raise,                             -- raise capital
    Issue,                             -- issue stock
    Sell                               -- sell stock
      : 'Action/Dir' ;

    -- Indirect object
    IssueAt,                           -- issue stock at fixed valuation
    SellAt
      : 'Action/Dir/Indir' ;

    -- Complements
    AComplDir   : 'Action/Dir' -> Term -> Action ;
    AComplIndir : 'Action/Indir' -> Term -> Action ;
    ASlashDir   : 'Action/Dir/Indir' -> Term -> 'Action/Indir' ; -- sell stock (at fixed valuation)
    ASlashIndir : 'Action/Dir/Indir' -> Term -> 'Action/Dir' ;   -- sell (stock) at fixed valuation

    -- Adjuncts: make an Action need another argument
    PursuantTo : Action -> 'Action/Indir' ;

    -- Negation of a whole Action: doesn't sell X / doesn't sell X and Y
    ANeg : Action -> Action ;

    -- Negation regarding the complements
    AComplNoneDir   : 'Action/Dir' -> [Term] -> Action ; -- sells neither X, Y nor Z
    AComplNoneIndir : 'Action/Indir' -> [Term] -> Action ; -- sells (X) neither to B nor to B

    -- Conjunctions

    BaseAD : 'Action/Dir' -> 'Action/Dir' -> ListActionDir ;
    ConsAD : 'Action/Dir' -> ListActionDir -> ListActionDir ;
    BaseAI : 'Action/Indir' -> 'Action/Indir' -> ListActionIndir ;
    ConsAI : 'Action/Indir' -> ListActionIndir -> ListActionIndir ;
    BaseADI : 'Action/Dir/Indir' -> 'Action/Dir/Indir' -> ListActionDirIndir ;
    ConsADI : 'Action/Dir/Indir' -> ListActionDirIndir -> ListActionDirIndir ;

    ConjAction : Conjunction -> [Action] -> Action ;
    ConjSlashDir : Conjunction -> ListActionDir -> 'Action/Dir' ;
    ConjSlashIndir : Conjunction -> ListActionIndir -> 'Action/Indir' ;
    ConjSlashDirIndir : Conjunction -> ListActionDirIndir -> 'Action/Dir/Indir' ;

  cat
    Temporality ;
--    Polarity ;

  fun
    TPresent  : Temporality ;
    TPast     : Temporality ;
    TFuture   : Temporality ;

    -- PPositive : Polarity ;
    -- PNegative : Polarity ;

    MAction : Temporality ->
      Term -> Action -> Move ; -- the company raises/raised/will raise capital

  ----------------
    -- Properties --
    ----------------
    Fixed,
    PreMoney,
    PostMoney,
    BonaFide,
    Voluntary : Property ;


    ForBenefit   -- general assignment for the benefit of the Company's creditors
      : Term -> Property ;

    WithPurpose   -- transaction with the purpose of raising capital
      : Action -> Property ;

    -----------
    -- Kinds --
    -----------
    Event,
    Capital,

    DissolutionEvent,
    Termination,
    GeneralAssignment,

    LiquidityEvent,
    ChangeOfControl,
    DirectListing,
    InitialPublicOffering,

    EquityFinancing,
    Transaction,
    PreferredStock,
    Valuation : Kind ;

    KWhetherOr  -- dissolution event, whether voluntary or involuntary
      : [Property] -> Kind -> Kind ;

    SingleOrSeries : Kind -> Kind ;

    --------------------------
    -- Kinds with arguments --
    --------------------------
  cat
    'Kind/Term' ;
    ListKindTerm ; -- winding up and dissolution (of the Company)

  fun
    Liquidation,
    Dissolution,
    WindingUp
      : 'Kind/Term' ;

    ComplKind : 'Kind/Term' -> Term -> Kind ; -- liquidation of the company

    BaseKT : 'Kind/Term' -> 'Kind/Term' -> ListKindTerm ;
    ConsKT : 'Kind/Term' -> ListKindTerm -> ListKindTerm ;
    ConjSlashTerm -- "liquidation and dissolution of the company"
     : Conjunction -> ListKindTerm -> 'Kind/Term' ;

    -----------
    -- Terms --
    -----------
  fun
    Company : Term ;
    Creditors : Term -> Term ; -- the Company's creditors

    TExcluding, -- liquidation of the Company, excluding a Liquidity Event
    TIncluding  -- fixed valuation, including a pre-money or post-money valuation
      : Determiner ->
      Kind -> Term ->
      Term ;

    RelIndir
      : Term ->       -- the contract
      Term ->         -- the Company
      Temporality ->  -- (will)
      'Action/Indir' -> -- sell(s) stock (under)
      Term ; -- the contract, under which the company sells stock

    RelDir
      : Term ->       -- the contract
      Term ->         -- the Company
      Temporality ->  -- (will)
      'Action/Dir' ->   -- sign(s)
      Term ; -- the contract, which the company signs/will sign


    --Series,   -- a series of transactions
    AnyOther  -- any other liquidation, dissolution or winding up
      : Determiner ;

}
