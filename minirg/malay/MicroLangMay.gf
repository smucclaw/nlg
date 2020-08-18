--# -path=.:../abstract

concrete MicroLangMay of MicroLang = open ResMay in {

  -- A very minimal concrete syntax.
  -- The categories are defined in ResMay, and so far they are all {s : Str}.

-----------------------------------------------------
---------------- Grammar part -----------------------
-----------------------------------------------------

  lincat
    Utt,    -- sentence, question, word...         e.g. "be quiet"
    S       -- declarative sentence                e.g. "she lives here"
     = {s : Str} ; -- these are often {s : Str} in the full RG as well

    Prep    -- preposition, or just case           e.g. "in", dative
      = Preposition ;

    Adv     -- adverbial phrase                    e.g. "in the house"
      = Adverbial ;

    Det     -- determiner phrase                   e.g. "those"
     = Determiner ;

    Pron    -- personal pronoun                    e.g. "she"
     = Pronoun ;

    N      -- common noun                         e.g. "house"
     =  Noun;
    CN     -- common noun (without determiner)    e.g. "red house"
     =  Noun;
    NP      -- noun phrase (subject or object)     e.g. "the red house"
     = {s : Str} ;

    VP,     -- verb phrase                         e.g. "lives here"
    Comp,   -- complement of copula                e.g. "warm"
    V,      -- one-place verb                      e.g. "sleep"
    V2,     -- two-place verb                      e.g. "love"
    AP,     -- adjectival phrase                   e.g. "warm"
    A       -- one-place adjective                 e.g. "warm"
      = Verb ;

  lin
-- Phrase
    -- : S  -> Utt ;         -- he walks
    UttS s = s ;

    -- : NP -> Utt ;         -- he
    UttNP np = np ;

-- Sentence
    -- : NP -> VP -> S ;             -- John walks
    PredVPS np vp = {s = np.s ++ vp.s ! Active} ;

-- Verb
    -- : V   -> VP ;             -- sleep
    UseV v = v ;

    -- : V2  -> NP -> VP ;       -- love it
    ComplV2 v2 np = {s = table {
      form => v2.s ! form ++ np.s}} ;

    -- : Comp  -> VP ;           -- be small
    UseComp comp = {s = comp.s} ;

    -- : AP  -> Comp ;           -- small
    CompAP ap = ap ;

    -- : VP -> Adv -> VP ;       -- sleep here
    AdvVP vp adv =  {s = table {
      form => vp.s ! form ++ adv.s}} ;

-- Noun
    -- : Det -> CN -> NP ;       -- this man
    DetCN det cn = {
      s = cn.s ! det.n ++ det.s ;
    } ;

    -- : Pron -> NP ;            -- she
    UsePron pron = pron ;

    -- : Det ;                   -- no articles: linearize as an empty string
    a_Det,
      the_Det = mkDet "" Singular ;

    aPl_Det,
      thePl_Det = mkDet "" Plural ;

    -- : Det ;                   -- this
    this_Det = mkDet "ini" Singular ;

    -- : Det ;                   -- that
    that_Det = mkDet "itu" Singular ;

    -- : Det ;                   -- these
    these_Det = mkDet "ini" Plural ;

    -- : Det ;                   -- those
    those_Det = mkDet "itu" Plural ;
    
    -- : N -> CN ;               -- house
    UseN n = n ;

    -- : AP -> CN -> CN ;        -- big house
    AdjCN ap cn = {s = table {
      num => cn.s ! num ++ ap.s ! Base}} ;

-- Adjective
    -- : A  -> AP ;              -- warm
    PositA a = a ;

-- Adverb
    -- : Prep -> NP -> Adv ;     -- in the house
    PrepNP prep np = {s = prep.s ++ np.s} ;

-- Structural
    -- : Prep ;
    in_Prep = mkPrep "di dalam" ;
    on_Prep = mkPrep "di atas" ;
    with_Prep = mkPrep "dengan" ;

    he_Pron   = mkPron "dia"   ;
    she_Pron  = mkPron "dia"  ;
    they_Pron = mkPron "mereka" ;


-----------------------------------------------------
---------------- Lexicon part -----------------------
-----------------------------------------------------

lin
{-  already_Adv = mkAdv "already" ;
  animal_N = mkN "animal" ;
  apple_N = mkN "apple" ;
  baby_N = mkN "baby" ;
  bad_A = mkA "bad" ;
  beer_N = mkN "beer" ;
  big_A = mkA "big" ;
  bike_N = mkN "bike" ;
  bird_N = mkN "bird" ;
  black_A = mkA "black" ;
  blood_N = mkN "blood" ;
  blue_A = mkA "blue" ;
  boat_N = mkN "boat" ;
  book_N = mkN "book" ;
  boy_N = mkN "boy" ;
  bread_N = mkN "bread" ;
  break_V2 = mkV2 "break" ;
  buy_V2  = mkV2 "buy" ;
  car_N = mkN "car" ;
  cat_N = mkN "cat" ;
  child_N = mkN "child" ;
  city_N = mkN "city" ;
  clean_A = mkA "clean" ;
  clever_A = mkA "clever" ;
  cloud_N = mkN "cloud" ;
  cold_A = mkA "cold" ;
  come_V = mkV "come" ;
-}
  computer_N = mkN "komputer" ;
  cow_N = mkN "lembu" ;
  dirty_A = mkA "kotor" ;
  dog_N = mkN "anjing" ;
  drink_V2 = mkV2 "minum" ; 
  eat_V2 = mkV2 "makan" ;
{-  find_V2 = mkV2 "find" ;
  fire_N = mkN "fire" ;
  fish_N = mkN "fish" ;
  flower_N = mkN "flower" ;
  friend_N = mkN "friend" ;
  girl_N = mkN "girl" ;
  good_A = mkA "good" ;
-}
  go_V = mkV "pergi" ;  
  grammar_N = mkN "tatabahasa" ;
  green_A = mkA "hijau" ;
{-  heavy_A = mkA "heavy" ;
  horse_N = mkN "horse" ;
  hot_A = mkA "hot" ;
  house_N = mkN "house" ;
  jump_V = mkV "jump" ;
  kill_V2 = mkV2 "kill" ;
  language_N = mkN "language" ;
  live_V = mkV "live" ;
  love_V2  = mkV2 "love" ;
  man_N = mkN "man" ;
  milk_N = mkN "milk" ;
  music_N = mkN "music" ;
  new_A = mkA "new" ; -}
  now_Adv = mkAdv "sekarang" ;
{-  old_A = mkA "old" ;
  play_V = mkV "play" ;
  read_V2  = mkV2 "read" ;
  ready_A = mkA "ready" ;
  red_A = mkA "red" ;
  river_N = mkN "river" ;
  run_V = mkV "run" ;
  sea_N = mkN "sea" ;
  see_V2  = mkV2 "see" ;
  ship_N = mkN "ship" ;
  sleep_V = mkV "sleep" ;
  small_A = mkA "small" ;
  star_N = mkN "star" ;
  swim_V = mkV "swim" ;
  teach_V2 = mkV2 "teach" ;
  train_N = mkN "train" ;
  travel_V = mkV "travel" ;
  tree_N = mkN "tree" ;
  understand_V2 = mkV2 "understand" ;
  wait_V2 = mkV2 "wait" ; -}
  walk_V = mkV "jalan" ;
{-  warm_A = mkA "warm" ;
  water_N = mkN "water" ;
  white_A = mkA "white" ;
  wine_N = mkN "wine" ;
  woman_N = mkN "woman" ;
  yellow_A = mkA "yellow" ;
  young_A = mkA "young" ; -}

}
