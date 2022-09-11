# ebnf

[![Go Reference](https://pkg.go.dev/badge/github.com/willfaught/ebnf.svg)](https://pkg.go.dev/github.com/willfaught/ebnf)

Package ebnf represents and parses a variant of [Extended Backus-Naur Form] called [Wirth Syntax Notation].
Terminal identifiers must begin with a lowercase letter.
Non-terminal identifiers must begin with an uppercase letter.
The first production defines the start non-terminal identifier.
Terminal identifiers are assumed to be defined elsewhere.

[Extended Backus-Naur Form]: https://en.wikipedia.org/wiki/Extended_Backus–Naur_form
[Wirth Syntax Notation]: https://en.wikipedia.org/wiki/Wirth_syntax_notation
