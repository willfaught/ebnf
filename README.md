# ebnf

Package ebnf represents and parses a variant of [Extended Backus-Naur Form](https://en.wikipedia.org/wiki/Extended_Backus–Naur_form) called [Wirth Syntax Notation](https://en.wikipedia.org/wiki/Wirth_syntax_notation). Terminal identifiers must begin with a lowercase letter. Nonterminal identifiers must begin with an uppercase letter. The first production defines the start nonterminal identifier. Terminal identifiers are assumed to be defined elsewhere and not cause conflicts. Epsilon is represented by an empty literal.

Grammars are written like so:

	Expression = Term {("+" | "-") Term}.
	Term = Factor {("*" | "/") Factor}.
	Factor = Number | "(" Expression ")".
	Number = Digit {Digit}.
	Digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9".

They can be parsed by [Parse](https://pkg.go.dev/github.com/willfaught/ebnf#Parse) into a [Grammar](https://pkg.go.dev/github.com/willfaught/ebnf#Grammar). [Grammar.Validate](https://pkg.go.dev/github.com/willfaught/ebnf#Grammar.Validate) determines whether a grammar is valid. [Grammar.First](https://pkg.go.dev/github.com/willfaught/ebnf#Grammar.First) and [Grammar.Follow](https://pkg.go.dev/github.com/willfaught/ebnf#Grammar.Follow) compute the first and follow sets for nonterminal identifiers. [Grammar.Conflict](https://pkg.go.dev/github.com/willfaught/ebnf#Grammar.Conflict) determines whether a valid grammar can be parsed by an LL(1) parser.
