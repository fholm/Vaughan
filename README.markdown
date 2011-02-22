F# implementation of a generic Top-Down-Operator-Precedence Parser 
as described in this paper http://portal.acm.org/citation.cfm?id=512931.

It has been extended to allow for statements in comparison to Pratt's
original algorithm which only parsed languages which use expression-only grammar.

The parser is "impure" in the sense that is uses a ref-cell for storing the
input in the T<_, _, _> record class, this is soley for performance reasons
as it's to expensive to create a new record object for every consumed token.
Certain functions also throw exceptions, which generally also is considered impure.

Pretty printed error messages are produced, such as:

Error on line: 5 col: 9
4: if(x == y) {
5:   print'x equals y');
----------^
Unexpected: #string "x equals y"

More information:
* http://en.wikipedia.org/wiki/Vaughan_Pratt (Original Inventor)
* http://en.wikipedia.org/wiki/Pratt_parser (Alias name)
* http://effbot.org/zone/simple-top-down-parsing.htm (Python implementation)
* http://javascript.crockford.com/tdop/tdop.html (JavaScript implementation)