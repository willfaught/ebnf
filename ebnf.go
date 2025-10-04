// Package ebnf represents and parses a variant of [Extended Backus-Naur Form] called [Wirth Syntax Notation].
// Terminal identifiers must begin with a lowercase letter.
// Nonterminal identifiers must begin with an uppercase letter.
// The first production defines the start nonterminal identifier.
// Terminal identifiers are assumed to be defined elsewhere and not cause conflicts.
// Epsilon is represented by an empty literal.
//
// Grammars are written like so:
//
//	Grammar = {Production}.
//	Production = Identifier "=" Expression ".".
//	Expression = Term {"|" Term}.
//	Term = Factor {Factor}.
//	Factor = Group | Identifier | Literal | Option | Repetition.
//	Group = "(" Expression ")".
//	Identifier = letter {letter}.
//	Literal = "\"" {character} "\"".
//	Option = "[" Expression "]".
//	Repetition = "{" Expression "}".
//
// They can be parsed by [Parse] into a [Grammar].
// [Grammar.Validate] determines whether a grammar is valid.
// [Grammar.First] and [Grammar.Follow] compute the first and follow sets for nonterminal identifiers.
// [Grammar.LL1] determines whether a valid grammar can be parsed by an LL(1) parser.
//
// [Extended Backus-Naur Form]: https://en.wikipedia.org/wiki/Extended_Backus–Naur_form
// [Wirth Syntax Notation]: https://en.wikipedia.org/wiki/Wirth_syntax_notation
package ebnf

import (
	"bytes"
	"fmt"
	"strings"
	"unicode"
	"unicode/utf8"
)

// Identifier is a terminal or nonterminal identifier.
// The text must not be empty.
type Identifier struct {
	Text string
}

// String returns the text.
func (i Identifier) String() string {
	return i.Text
}

// Literal is the content of a quoted string.
// If the text is empty, it represents epsilon, the empty string.
type Literal struct {
	Text string
}

// String returns the text surrounded by double quotes.
func (l Literal) String() string {
	return fmt.Sprintf("%q", l.Text)
}

// Factor is a term sequence.
// One, and only one, of the fields must not be nil.
type Factor struct {
	Group      *Expression
	Identifier *Identifier
	Literal    *Literal
	Option     *Expression
	Repetition *Expression
}

// String returns a group string surrounded by round brackets,
// an option string surrounded by square brackets,
// a repetition string surrounded by curly brackets,
// and an identifier or literal string as itself.
func (f Factor) String() string {
	switch {
	case f.Group != nil:
		return fmt.Sprintf("(%v)", f.Group)
	case f.Identifier != nil:
		return f.Identifier.String()
	case f.Literal != nil:
		return f.Literal.String()
	case f.Option != nil:
		return fmt.Sprintf("[%v]", f.Option)
	case f.Repetition != nil:
		return fmt.Sprintf("{%v}", f.Repetition)
	default:
		panic(f)
	}
}

// Term is an expression alternative.
// There must be at least one factor.
// All factors must not be nil.
type Term struct {
	Factors []*Factor
}

// String returns the factor strings joined by a space.
func (t Term) String() string {
	ss := make([]string, len(t.Factors))
	for i, f := range t.Factors {
		ss[i] = fmt.Sprint(f)
	}
	return strings.Join(ss, " ")
}

// Expression is the right side of a production.
// There must be at least one term.
// All terms must not be nil.
type Expression struct {
	Terms []*Term
}

// String returns the term strings joined by " | ".
func (e Expression) String() string {
	ss := make([]string, len(e.Terms))
	for i, t := range e.Terms {
		ss[i] = fmt.Sprint(t)
	}
	return strings.Join(ss, " | ")
}

// Production is a grammar production.
// The identifier and expression must not be nil.
type Production struct {
	Identifier *Identifier
	Expression *Expression
}

// String returns the identifier and expression separated by " = ",
// followed by a period.
func (p Production) String() string {
	return fmt.Sprintf("%s = %v.", p.Identifier, p.Expression)
}

// Grammar is an abstract syntax tree for a grammar.
// There must be at least one production.
type Grammar struct {
	Productions []*Production
}

// Error has all parse errors.
type Error struct {
	Errors []error
}

// Error returns all the error strings joined by a line feed.
func (e Error) Error() string {
	ss := make([]string, len(e.Errors))
	for i, err := range e.Errors {
		ss[i] = err.Error()
	}
	return strings.Join(ss, "\n")
}

// Parse returns a Grammar for a valid grammar, or an error otherwise.
func Parse(s string) (*Grammar, error) {
	p := newParser(bytes.NewBufferString(s))
	g := p.parseGrammar()
	var errs []error
	if len(p.lexer.errs) > 0 {
		errs = p.lexer.errs
	}
	if len(p.errs) > 0 {
		errs = append(errs, p.errs...)
	}
	if len(errs) > 0 {
		return nil, Error{Errors: errs}
	}
	return g, nil
}

func merge(from, to map[any]struct{}) {
	for k, v := range from {
		to[k] = v
	}
}

func terminal(i *Identifier) bool {
	r, _ := utf8.DecodeRuneInString(i.Text)
	return unicode.IsLower(r)
}

func first(all map[any]map[any]struct{}, item any) map[any]struct{} {
	var this map[any]struct{}
	switch item := item.(type) {
	case nil:
	case *Expression:
		if item == nil {
			break
		}
		this = map[any]struct{}{}
		for _, t := range item.Terms {
			merge(first(all, t), this)
		}
		all[item] = this
	case *Factor:
		if item == nil {
			break
		}
		this = map[any]struct{}{}
		merge(first(all, item.Group), this)
		merge(first(all, item.Identifier), this)
		merge(first(all, item.Literal), this)
		merge(first(all, item.Option), this)
		merge(first(all, item.Repetition), this)
		if item.Option != nil || item.Repetition != nil {
			this[Literal{}] = struct{}{}
		}
		all[item] = this
	case *Identifier:
		if item == nil {
			break
		}
		this = map[any]struct{}{*item: {}}
		all[*item] = this
	case *Literal:
		if item == nil {
			break
		}
		this = map[any]struct{}{*item: {}}
		all[*item] = this
	case *Production:
		if item == nil {
			break
		}
		this = first(all, item.Expression)
		all[item] = this
	case *Term:
		if item == nil {
			break
		}
		for _, f := range item.Factors {
			first(all, f)
		}
		nonterminal := false
		for t := range all[item.Factors[0]] {
			if i, ok := t.(Identifier); ok && !terminal(&i) {
				nonterminal = true
				break
			}
		}
		if nonterminal {
			this = map[any]struct{}{item: {}}
		} else {
			this = firstFactors(all, item.Factors)
		}
		all[item] = this
	}
	return this
}

func firstFactors(all map[any]map[any]struct{}, fs []*Factor) map[any]struct{} {
	this := all[fs[0]]
	if len(fs) == 1 {
		return this
	}
	if _, ok := this[Literal{}]; !ok {
		return this
	}
	delete(this, Literal{})
	merge(firstFactors(all, fs[1:]), this)
	return this
}

// First returns the first terminals of a valid grammar.
// The map keys are [*Production], [*Expression], [*Term], [*Factor], [Identifier], or [Literal] values.
// The map values are sets of [Identifier] and [Literal] values.
func (g Grammar) First() map[any]map[any]struct{} {
	all := map[any]map[any]struct{}{}
	for _, p := range g.Productions {
		this := first(all, p)
		delete(this, *p.Identifier)
		all[*p.Identifier] = this
	}
	for {
		var merged bool
		for _, terminals := range all {
			for t := range terminals {
				if i, ok := t.(Identifier); ok && !terminal(&i) {
					merge(all[i], terminals)
					delete(terminals, i)
					merged = true
				}
			}
		}
		if !merged {
			break
		}
	}
	for _, terminals := range all {
		for terminal := range terminals {
			if term, ok := terminal.(*Term); ok {
				delete(terminals, terminal)
				merge(firstFactors(all, term.Factors), terminals)
			}
		}
	}
	return all
}

// FirstNonterminals returns the first terminals of the nonterminals of a valid grammar.
func (g Grammar) FirstNonterminals() map[string]map[any]struct{} {
	first := map[string]map[any]struct{}{}
	for k, v := range g.First() {
		if p, ok := k.(*Production); ok {
			first[p.Identifier.Text] = v
		}
	}
	return first
}

func firstFactorsCopy(first map[any]map[any]struct{}, fs []*Factor) map[any]struct{} {
	this := first[fs[0]]
	if len(fs) == 1 {
		return this
	}
	if _, ok := this[Literal{}]; !ok {
		return this
	}
	copy := make(map[any]struct{}, len(this))
	for k, v := range this {
		copy[k] = v
	}
	delete(this, Literal{})
	merge(firstFactorsCopy(first, fs[1:]), copy)
	return copy
}

func follow(first map[any]map[any]struct{}, all map[string]map[any]struct{}, p *Production, item any) {
	switch item := item.(type) {
	case nil:
	case *Expression:
		if item == nil {
			break
		}
		for _, t := range item.Terms {
			follow(first, all, p, t)
		}
	case *Factor:
		if item == nil {
			break
		}
		follow(first, all, p, item.Group)
		follow(first, all, p, item.Identifier)
		follow(first, all, p, item.Literal)
		follow(first, all, p, item.Option)
		follow(first, all, p, item.Repetition)
	case *Identifier:
	case *Literal:
	case *Production:
		if item == nil {
			break
		}
		if all[item.Identifier.Text] == nil {
			all[item.Identifier.Text] = map[any]struct{}{}
		}
		follow(first, all, item, item.Expression)
	case *Term:
		if item == nil {
			break
		}
		for i, f := range item.Factors {
			if f.Identifier == nil || terminal(f.Identifier) {
				follow(first, all, p, f)
				continue
			}
			this := all[f.Identifier.Text]
			if this == nil {
				this = map[any]struct{}{}
				all[f.Identifier.Text] = this
			}
			if i == len(item.Factors)-1 {
				merge(map[any]struct{}{*p.Identifier: {}}, this)
			} else {
				rest := firstFactorsCopy(first, item.Factors[i+1:])
				if _, ok := rest[Literal{}]; ok {
					delete(rest, Literal{})
					rest[*p.Identifier] = struct{}{}
				}
				merge(rest, this)
			}
		}
	}
}

func (g Grammar) follow(first map[any]map[any]struct{}) map[string]map[any]struct{} {
	all := map[string]map[any]struct{}{}
	for _, p := range g.Productions {
		follow(first, all, nil, p)
		delete(all[p.Identifier.Text], *p.Identifier)
	}
	for {
		var merged bool
		for _, terminals := range all {
			for t := range terminals {
				if i, ok := t.(Identifier); ok && !terminal(&i) {
					existing := all[i.Text]
					merge(existing, terminals)
					delete(terminals, t)
					merged = true
				}
			}
		}
		if !merged {
			break
		}
	}
	return all
}

// Follow returns the follow terminals of a valid grammar.
// The map keys are nonterminal [Identifier] text values.
// The map values are sets of [Identifier] and [Literal] values.
func (g Grammar) Follow() map[string]map[any]struct{} {
	return g.follow(g.First())
}

// FirstFirstConflictError is a first/first LL(1) grammar parse conflict.
type FirstFirstConflictError struct {
	Nonterminal string
	Terminal    any // an Identifier or Literal
}

// Error indicates a first/first conflict exists for an LL(1) parser
func (f FirstFirstConflictError) Error() string {
	var kind, content string
	switch t := f.Terminal.(type) {
	case Identifier:
		kind = "identifier"
		content = t.Text
	case Literal:
		kind = "literal"
		content = t.Text
	default:
		panic(f.Terminal)
	}
	return fmt.Sprintf("first/first conflict for %s %q for nonterminal %q", kind, content, f.Nonterminal)
}

func (g Grammar) firstFirstConflict(first map[any]map[any]struct{}) error {
	for _, p := range g.Productions {
		terminals := map[any]struct{}{}
		for _, t := range p.Expression.Terms {
			for terminal := range first[t] {
				if _, ok := terminals[terminal]; ok {
					return FirstFirstConflictError{Nonterminal: p.Identifier.Text, Terminal: terminal}
				}
				terminals[terminal] = struct{}{}
			}
		}
	}
	return nil
}

// FirstFollowConflictError is a first/follow LL(1) grammar parse conflict.
type FirstFollowConflictError struct {
	Nonterminal string
	Terminal    any // an Identifier or Literal
}

func (f FirstFollowConflictError) Error() string {
	var kind, content string
	switch t := f.Terminal.(type) {
	case Identifier:
		kind = "identifier"
		content = t.Text
	case Literal:
		kind = "literal"
		content = t.Text
	default:
		panic(f.Terminal)
	}
	return fmt.Sprintf("first/follow conflict for %s %q for nonterminal %q", kind, content, f.Nonterminal)
}

func (g Grammar) firstFollowConflict(first map[any]map[any]struct{}, follow map[string]map[any]struct{}) error {
	for _, p := range g.Productions {
		thisFirst := first[*p.Identifier]
		if _, ok := thisFirst[Literal{}]; !ok {
			continue
		}
		thisFollow := follow[p.Identifier.Text]
		var smaller, larger map[any]struct{}
		if len(thisFirst) < len(thisFollow) {
			smaller = thisFirst
			larger = thisFollow
		} else {
			smaller = thisFollow
			larger = thisFirst
		}
		for x := range smaller {
			if _, ok := larger[x]; ok {
				return FirstFollowConflictError{Nonterminal: p.Identifier.Text, Terminal: x}
			}
		}
	}
	return nil
}

// LL1 returns whether a valid grammar would have a first/first or first/follow conflict for an LL(1) parser.
// Either a FirstFirstConflictError, a FirstFollowConflictError, or nil are returned.
func (g Grammar) LL1() error {
	first := g.First()
	follow := g.follow(first)
	if err := g.firstFirstConflict(first); err != nil {
		return err
	}
	return g.firstFollowConflict(first, follow)
}

// String returns the production strings joined by a line feed.
func (g Grammar) String() string {
	ss := make([]string, len(g.Productions))
	for i, p := range g.Productions {
		ss[i] = fmt.Sprint(p)
	}
	return strings.Join(ss, "\n")
}

func traverse(item any, visit func(any)) {
	if item == nil {
		return
	}
	visit(item)
	switch item := item.(type) {
	case *Expression:
		for _, t := range item.Terms {
			visit(t)
			traverse(t, visit)
		}
	case *Factor:
		if item.Group != nil {
			visit(item.Group)
			traverse(item.Group, visit)
		}
		if item.Identifier != nil {
			visit(item.Identifier)
			traverse(item.Identifier, visit)
		}
		if item.Literal != nil {
			visit(item.Literal)
			traverse(item.Literal, visit)
		}
		if item.Option != nil {
			visit(item.Option)
			traverse(item.Option, visit)
		}
		if item.Repetition != nil {
			visit(item.Repetition)
			traverse(item.Repetition, visit)
		}
	case *Grammar:
		for _, p := range item.Productions {
			visit(p)
			traverse(p, visit)
		}
	case *Identifier:
	case *Literal:
	case *Production:
		// item.Identifier is not visited or traversed for validation reasons.
		visit(item.Expression)
		traverse(item.Expression, visit)
	case *Term:
		for _, f := range item.Factors {
			visit(f)
			traverse(f, visit)
		}
	}
}

// Validate checks that production identifiers are capitalized, defined, and used.
func (g Grammar) Validate() error {
	var errs []error
	used := map[string]bool{}
	traverse(&g, func(item any) {
		switch item := item.(type) {
		case *Grammar:
			for _, p := range item.Productions {
				if p.Identifier != nil && len(p.Identifier.Text) > 0 && !terminal(p.Identifier) {
					if _, ok := used[p.Identifier.Text]; ok {
						errs = append(errs, fmt.Errorf("identifier %q is defined twice", p.Identifier.Text))
						continue
					}
					used[p.Identifier.Text] = false
				}
			}
		case *Identifier:
			if !terminal(item) {
				b, ok := used[item.Text]
				if !ok {
					errs = append(errs, fmt.Errorf("identifier %q is undefined", item.Text))
					return
				}
				if !b {
					used[item.Text] = true
				}
			}
		case *Production:
			if terminal(item.Identifier) {
				errs = append(errs, fmt.Errorf("identifier %q starts with a lowercase character", item.Identifier.Text))
				return
			}
		}
	})
	for ident, b := range used {
		if ident != g.Productions[0].Identifier.Text && !b {
			errs = append(errs, fmt.Errorf("identifier %q is unused", ident))
		}
	}
	if len(errs) > 0 {
		return Error{Errors: errs}
	}
	return nil
}
