# SAFE

GF grammar for a small fragment of post-money SAFE.

## Disambiguation in Haskell

To run the disambiguation program, you need to have GF. Then, compile the PGF:

    gf -make SAFEQueryEng.gf
  
Then just do

    stack build    

### Example run
```
$ stack run disambiguate
…
Write your sentence here. If it's ambiguous, I will ask for clarification.
Write quit to exit.
> capital means capital
The input is not ambiguous.
MDefTerm Capital (TDet ASg Capital)
> equity financing means a bona fide transaction or series of transactions with the principal purpose of raising capital , pursuant to which the Company issues and sells preferred stock at a fixed valuation , including a pre-money or post-money valuation
The sentence has the following interpretations:
* the Company issues and sells preferred stock at a valuation pursuant to a transaction .
* the Company issues and sells preferred stock at a valuation pursuant to capital .
```
