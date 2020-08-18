resource ResMay = {

  -- This is a /resource module/. It is neither an abstract nor a concrete syntax,
  -- but instead it contains /opers/, which can be used in a concrete syntax module.

  -- So far, all of the types are just a record with string. This will change as we
  -- add necessary distinctions to the grammar.
oper

-- Determiners
  Determiner : Type = {s : Str; n: Number} ;

  mkDet : Str -> Number -> Determiner ;
  mkDet str num = {s = str; n = num} ;

-- Nouns
  Noun : Type = {s : Number => Str} ;

  mkN : Str -> Noun ;
  mkN str = {s = table {
    Singular => str;
    Plural => str + "-" + str }
  } ;

param

  Number = Singular | Plural ;

oper



-- Pronouns
  Pronoun : Type = {s : Str} ;

  mkPron : Str -> Pronoun ;
  mkPron str = {s = str} ;

  -- Verbs
  Verb : Type = {s : Verbform => Str} ;

  param
    Verbform = Active | Base ;  
  oper

  mkV : Str -> Verb ;
  mkV str = {s = table {
    Base => str;
    Active => case str of { 
      ("m" | "p" | "b") + _ => "me" + str;
      _ => "ber" + str 
    } 
  }} ;

  mkV2 : Str -> Verb ;
  mkV2 = mkV ;

  -- Adjectives are verbs, as per
  -- https://en.wikipedia.org/wiki/Malay_grammar#Descriptive_stative_verbs
  mkA : Str -> Verb ;
  mkA str = {s = \\_=> str} ;

  -- Adverbials
  Adverbial : Type = {s : Str} ;

  mkAdv : Str -> Adverbial ;
  mkAdv str = {s = str} ;

  -- Prepositions
  Preposition : Type = {s : Str} ;

  mkPrep : Str -> Preposition ;
  mkPrep str = {s = str} ;

}
