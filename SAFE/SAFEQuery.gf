abstract SAFEQuery = Query ** {
  flags startcat=Move ;

  cat
    Action ;
    'Action_Dir' ;
    'Action_Indir' ;
    'Action_Dir_Indir' ;
    [Action]{2} ;              -- sells stock to Acme and raises capital
    ['Action_Dir']{2} ;        -- sells today and issues (stock) at fixed valuation
    ['Action_Indir']{2} ;      -- sells widgets and issues stock (at fixed valuation)
    ['Action_Dir_Indir']{2} ;  -- sells and issues (stock) (at fixed valuation)

  fun
    -------------
    -- Actions --
    -------------
    -- Direct object
    Raise,                             -- raise capital
    Issue,                             -- issue stock
    Sell                               -- sell stock
      : 'Action_Dir' ;

    -- Indirect object
    IssueAt,                           -- issue stock at fixed valuation
    SellAt
      : 'Action_Dir_Indir' ;

    -- Complements
    AComplDir   : 'Action_Dir' -> Term -> Action ;
    AComplIndir : 'Action_Indir' -> Term -> Action ;
    ASlashDir   : 'Action_Dir_Indir' -> Term -> 'Action_Indir' ; -- sell stock (at fixed valuation)
    ASlashIndir : 'Action_Dir_Indir' -> Term -> 'Action_Dir' ;   -- sell (stock) at fixed valuation

    -- Adjuncts: make an Action need another argument
    PursuantTo : Action -> 'Action_Indir' ;

    -- Negation of a whole Action: doesn't sell X _ doesn't sell X and Y
    ANeg : Action -> Action ;

    -- Negation regarding the complements
    AComplNoneDir   : 'Action_Dir' -> [Term] -> Action ; -- sells neither X, Y nor Z
    AComplNoneIndir : 'Action_Indir' -> [Term] -> Action ; -- sells (X) neither to B nor to B

    -- Conjunctions

    ConjAction : Conjunction -> [Action] -> Action ;
    ConjSlashDir : Conjunction -> ['Action_Dir'] -> 'Action_Dir' ;
    ConjSlashIndir : Conjunction -> ['Action_Indir'] -> 'Action_Indir' ;
    ConjSlashDirIndir : Conjunction -> ['Action_Dir_Indir'] -> 'Action_Dir_Indir' ;

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
      Term -> Action -> Move ; -- the company raises_raised_will raise capital

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
    'Kind_Term' ;
    ['Kind_Term']{2} ;

  fun
    Liquidation,
    Dissolution,
    WindingUp
      : 'Kind_Term' ;

    ComplKind : 'Kind_Term' -> Term -> Kind ; -- liquidation of the company

    ConjSlashTerm -- "liquidation and dissolution of the company"
     : Conjunction -> ['Kind_Term'] -> 'Kind_Term' ;

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
      'Action_Indir' -> -- sells stock (under)
      Term ; -- the contract, under which the company sells stock

    RelDir
      : Term ->       -- the contract
      Term ->         -- the Company
      Temporality ->  -- (will)
      'Action_Dir' ->   -- signs
      Term ; -- the contract, which the company signs_will sign


    --Series,   -- a series of transactions
    AnyOther  -- any other liquidation, dissolution or winding up
      : Determiner ;

}
