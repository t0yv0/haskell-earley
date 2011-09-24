# hasekll-earley

The code implements Earley parsing algorithm in Haskell [1].
Context-free grammars and processing rules are specified directly in
Haskell, without templates or other forms of pre-processing, using the
monadic syntax with the DoRec extension.  Programming in this style
feels like using parser combinators but does not prevent global
grammar analysis.

Warning: This code is alpha quality, as I have not yet convinced
myself that the implementation is correct, especially with handling
epsilon rules. Nor did I do any complexity

[1]: http://webhome.cs.uvic.ca/~nigelh/Publications/PracticalEarleyParsing.pdf
