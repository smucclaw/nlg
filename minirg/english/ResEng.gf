resource ResEng = {

  -- Determiners
  param
    Number = Sg | Pl ;

  oper
    Determiner : Type = {s : Str ; n : Number} ;

    mkDet : Str -> Number -> Determiner ;
    mkDet   str    num    =  {s = str ; n = num} ;

    -- Nouns
    Noun : Type = {s : Number => Str} ;

    mkN = overload {
      mkN : Str -> Noun = regN ;
      mkN : Str -> Str -> Noun = worstN ;
      } ;

    worstN : Str -> Str -> Noun ;
    worstN   sg     pl  = {
      s = table { Sg => sg ;
		  Pl => pl }
      } ;

    regN : Str -> Noun ;
    regN sg =
      let pl : Str = case sg of {
  	    b + ("a"|"e"|"i"|"o"|"u") + "y"
              => sg + "s" ;
  	    bab + "y" => bab + "ies" ;
  	    _         => sg + "s"
    	    } ;
      in worstN sg pl ;

  -- Verbs
  param
    Agr = Sg3 | Other ;

  oper
    Verb : Type = {s : Agr => Str} ;

    mkV : Str -> Str -> Verb ;
    mkV go goes = {
      s = table {
        Sg3 => goes ;
        Other => go}
      } ;

    mkV2 : Str -> Verb ;
    mkV2 teach =
      let teaches : Str = case teach of {
            _ + ("s"|"sh"|"ch")
              => teach + "es" ;
            _ => teach + "s" } ;
      in {s = table {
            Sg3 => teaches ;
            Other => teach }
      } ;


  -- Pronouns, noun phrases
  param
    Case = Nom | Acc ;
  oper
    NounPhrase : Type = {s : Case => Str ; a : Agr} ;

    mkPron : Str -> Str -> Agr -> NounPhrase ;
    mkPron nom acc agr = {
      s = table {
        Nom => nom ;
        Acc => acc } ;
      a = agr
      } ;

    mkA : Str -> {s : Str} ;
    mkA str = {s = str} ;

    -- Named constructors for other categories
    -- even though they are all just {s : Str}
    mkAdv : Str -> {s : Str} ;
    mkAdv str = {s = str} ;

    mkPrep : Str -> {s : Str}  ;
    mkPrep str = {s = str} ;
}
