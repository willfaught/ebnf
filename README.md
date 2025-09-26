# ebnf

Package ebnf represents and parses a variant of [Extended Backus-Naur Form] called
[Wirth Syntax Notation]. Terminal identifiers must begin with a lowercase letter.
Non-terminal identifiers must begin with an uppercase letter. The first
production defines the start non-terminal identifier. Terminal identifiers are
assumed to be defined elsewhere. Epsilon is represented by an empty literal.

Grammars are written like so:

    Expression = Term { ( "+" | "-" ) Term } .
    Term = Factor { ( "*" | "/" ) Factor } .
    Factor = Number | "(" Expression ")" .
    Number = Digit { Digit } .
    Digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" .

They can be parsed by Parse into a Grammar. Grammar.Validate determines whether
a grammar is valid. Grammar.First and Grammar.Follow compute the first and
follow sets for non-terminal identifiers. Grammar.Conflict determines whether a
valid grammar can be parsed by an LL(1) parser.

[Extended Backus-Naur Form]: https://en.wikipedia.org/wiki/Extended_Backus–Naur_form
[Wirth Syntax Notation]: https://en.wikipedia.org/wiki/Wirth_syntax_notation
