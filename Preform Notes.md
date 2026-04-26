Preform

So it's a fairly standard BNF-like grammar. There's nonterminals (NT) and productions and so forth.

Each NT is a list of productions (production_list, actually - one per language). As long as I choose to avoid needing to actually parse preform and just write it in haskell DSLs then I can skip right ahead to representing it.

A production is a list of ptokens with some optimisation info (see https://ganelson.github.io/inform/words-module/4-lp.html#SP11)
Each ptoken is one of:
- single wildcard (one word) - ###
- multiple wildcard (nonempty) - ...
- multiple wildcard but wikth balanced parentheses - ...... (6 dots)
- optional wildcard (possibly empty) - ***

- nonterminal
- a singular word
then it can be negated, or exclude uppercase, or have a list of alternatives

it has some amount of results - NT results, range start/ends but idk what exactly. also more optimisation and instrumentation info

A range is the words you want to return from a wildcard or a { } range; the example is
`make ... from {rice ... onions} and peppers`
'the first ... ptoken has start and end set to 1; rice has start 2; onions has end 2. Note that the second ..., inside the braces, doesn't start or end anything'

then there's a bunch (14.1 onwards) of the parsing of a list into a structure with ranges
notably it will "create" nonexistent nonterminals.

then onto the optimiser

optimising NTs

the NTI constraint and some length bounds
productions also have these same constraint/length, but also a bunch of "strut" info
struts are ptoken ranges where the start/end is not known, excluding wildcards. struts do have lengths
we can tag ptokens with a position (maybe), or part of their strut (which doesn't include a position)
and if it's "fast" (fixed known position not ending a bracing)

obviously to evaluate them all, you just loop and recurse.
forward pass to calculate locations of ptokens
then backward pass to calculate locations of ptokens (which don't have a front position)

a strut is inelastic (always of length N) and so we can find em by iterating again.
it's elastic if the min/max are not the same.
interestingly the min/max of a negated nonterminal are infinite?

then we get to NTIs. Every NT has one of 26 bits associated to it, then vocab entries have 26 incidence bits.
a 1 bit in the vocab entry = a nonterminal with that incidence bit has this word in it somewhere.

the actual comparison is between a word (with its many NTI bits) and an NTI constraint, which is a collection of
requirement NTI bits - or, and, first and one of/all of. Disjunction (or) means that at least one word must
match. "For example, suppose a NTIC has DS_req set to 0x280 — i.e., to a bitmap in which bits 7 and 9 are set (counting upwards from 0). This is then saying that a word range such as "sense and prejudice" can only be a match if one of the three words "sense", "and" or "prejudice" has both bits 7 and 9 set."
it can also just be "ignore everything"

Then we get into making these NTICs.
- unconstrained
- every word must match some bit
- concatenate (X then Y implies an Xntic and a Yntic, which makes a new XthenYntic)
- disjoin (X or Y gives an XorYntic)
- simplify

then we build up our incidence bits. for each production, for each ptoken, if it's a fixed word and not a negation, we mark that word with the bit in the vocab map. for an NT it's a disjunction of the productions, for a production it's a concatenation of the ptokens

then we work out every other ptoken. there's some 0 width ones (ignore), the fixed words are a each word must have, and nonterminal recursion sets it to that (plus optimisation).
