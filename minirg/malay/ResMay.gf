resource ResMay = {

  -- This is a /resource module/. It is neither an abstract nor a concrete syntax,
  -- but instead it contains /opers/, which can be used in a concrete syntax module.

  -- So far, all of the types are just a record with string. This will change as we
  -- add necessary distinctions to the grammar.
oper

-- Determiners
  Determiner : Type = {s : Str} ;

  mkDet : Str -> Determiner ;
  mkDet str  = {s = str} ;

-- Nouns
  Noun : Type = {s : Str} ;

  mkN : Str -> Noun ;
  mkN str = {s = str} ;

-- Pronouns
  Pronoun : Type = {s : Str} ;

  mkPron : Str -> Pronoun ;
  mkPron str = {s = str} ;

  -- Verbs
  Verb : Type = {s : Str} ;

  mkV : Str -> Verb ;
  mkV str = {s = str} ;

  mkV2 : Str -> Verb ;
  mkV2 str = {s = str} ;

  -- Adjectives are verbs, as per
  -- https://en.wikipedia.org/wiki/Malay_grammar#Descriptive_stative_verbs
  mkA : Str -> Verb ;
  mkA str = {s = str} ;

  -- Adverbials
  Adverbial : Type = {s : Str} ;

  mkAdv : Str -> Adverbial ;
  mkAdv str = {s = str} ;

  -- Prepositions
  Preposition : Type = {s : Str} ;

  mkPrep : Str -> Preposition ;
  mkPrep str = {s = str} ;

}
