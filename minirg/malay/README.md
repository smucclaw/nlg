# Malay

This is a conrete syntax for Malay. (So far only for MicroLang, not MiniLang.)

TODO: what is the relevant ISO code? I chose [this one](https://iso639-3.sil.org/code/may) but feel free to change, if something else fits better.

## How to run

In this directory (`nlg/minirg/malay`), run

```
$ gf
         *  *  *
      *           *
    *               *
   *
   *
   *        * * * * * *
   *        *         *
    *       * * * *  *
      *     *      *
         *  *  *

This is GF version 3.10.4.

Languages:
```

Now you're in the GF shell. Next we need to open some grammars.

```
> i MicroLangMay.gf ../english/MicroLangEng.gf
linking ... OK

Languages: MicroLangEng MicroLangMay
```

Now you have loaded English and Malay. To parse and linearize, use the commands `p` and `l`. For example:

```
MicroLang>  p "she eats grammars" | l
she eats green grammars
dia makan tatabahasa
```

There are only a couple of words added in the Malay lexicon, so that's pretty much everything you can say at the moment. But extend the lexicon and the grammar rules, and then you can say more things.
