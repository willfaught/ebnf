// Package ebnf represents and parses a variant of [Extended Backus-Naur Form] called [Wirth Syntax Notation].
// Terminal identifiers must begin with a lowercase letter.
// Non-terminal identifiers must begin with an uppercase letter.
// The first production defines the start non-terminal identifier.
// Terminal identifiers are assumed to be defined elsewhere.
// Epsilon is represented by an empty literal.
//
// [Extended Backus-Naur Form]: https://en.wikipedia.org/wiki/Extended_Backus–Naur_form
// [Wirth Syntax Notation]: https://en.wikipedia.org/wiki/Wirth_syntax_notation
package ebnf

/*
Grammar methods:
list of terminal syms
list of nonterm syms
for each nonterm sym, the sets of its start and follow syms
based on these 3, determine if given syntax can be parsed top down with lookahead of 1 sym
show conflicting productions if not
*/

// func DeterministicL1(g *Grammar) bool {
// A | B: first(A) and first(B) must be disjoint
// A B: if empty sequence in A, then first(A) and first(B) must be disjoint
// [A] B C..., {A} B C...: first(A) must be disjoint from first(B), first(C), etc..., anything that follows [A]/{A}
// No left recursion: A = A...
// symbol sets: first, follow
// return false
// }

import (
	"bytes"
	"fmt"
	"io"
	"strings"
	"unicode"
	"unicode/utf8"
)

var tokenString = map[token]string{
	lparen: "(",
	lbrak:  "[",
	lbrace: "{",
	bar:    "|",
	eql:    "=",
	rparen: ")",
	rbrak:  "]",
	rbrace: "}",
	period: ".",
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

func merge(from, to map[any]struct{}) {
	for k, v := range from {
		to[k] = v
	}
}

func terminal(i *Identifier) bool {
	r, _ := utf8.DecodeRuneInString(i.Text)
	return unicode.IsLower(r)
}

func tokenName(t token, text string) string {
	var s string
	switch t {
	case ident:
		s = "identifier"
	case literal:
		s = "literal"
	case other:
		s = fmt.Sprintf("%q", text)
	default:
		s2, ok := tokenString[t]
		if !ok {
			panic(t)
		}
		s = fmt.Sprintf("%q", s2)
	}
	return s
}

func tokenValue(t token, text string) string {
	var s string
	switch t {
	case ident, literal, other:
		s = fmt.Sprintf("%q", text)
	default:
		s2, ok := tokenString[t]
		if !ok {
			panic(t)
		}
		s = fmt.Sprintf("%q", s2)
	}
	return s
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

// Error has one or more errors.
type Error struct {
	Errors []error
}

// Error returns all the error strings joined by a newline.
func (e Error) Error() string {
	ss := make([]string, len(e.Errors))
	for i, err := range e.Errors {
		ss[i] = err.Error()
	}
	return strings.Join(ss, "\n")
}

// Expression is the right side of a production.
// There must be at least one term.
// All terms must not be nil.
type Expression struct {
	Terms []*Term
}

func (e Expression) String() string {
	ss := make([]string, len(e.Terms))
	for i, t := range e.Terms {
		ss[i] = fmt.Sprint(t)
	}
	return strings.Join(ss, " | ")
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

// Grammar is an abstract syntax tree for a grammar.
// There must be at least one production.
type Grammar struct {
	Productions []*Production
}

func (g Grammar) String() string {
	ss := make([]string, len(g.Productions))
	for i, p := range g.Productions {
		ss[i] = fmt.Sprint(p)
	}
	return strings.Join(ss, "\n")
}

// Parse returns a Grammar for a valid grammar, or an error otherwise.
func Parse(s string) (*Grammar, error) {
	p := newParser(bytes.NewBufferString(s))
	g := p.grammar()
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

// First returns the first terminals of a valid grammar.
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

// FirstNonterminals returns the first terminals of a valid grammar for its non-terminals.
func (g Grammar) FirstNonterminals() map[string]map[any]struct{} {
	first := map[string]map[any]struct{}{}
	for k, v := range g.First() {
		if p, ok := k.(*Production); ok {
			first[p.Identifier.Text] = v
		}
	}
	return first
}

// Follow returns the follow terminals of a valid grammar.
func (g Grammar) Follow() map[string]map[any]struct{} {
	return g.follow(g.First())
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

// Conflict returns whether a valid grammar has a first/first or first/follow conflict for an LL(1) parser.
func (g Grammar) Conflict() error {
	first := g.First()
	follow := g.follow(first)
	if err := g.firstFirstConflict(first); err != nil {
		return err
	}
	return g.firstFollowConflict(first, follow)
}

type FirstFirstConflictError struct {
	Nonterminal string
	Terminal    any
}

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
	return fmt.Sprintf("first/first conflict for %s %q for non-terminal %q", kind, content, f.Nonterminal)
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

type FirstFollowConflictError struct {
	Nonterminal string
	Terminal    any
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
	return fmt.Sprintf("first/follow conflict for %s %q for non-terminal %q", kind, content, f.Nonterminal)
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

// Validate checks that production identifiers are capitalized and defined.
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

// Identifier is a terminal or non-terminal identifier.
// The text must not be empty.
type Identifier struct {
	Text string
}

func (i Identifier) String() string {
	return i.Text
}

// Literal is the content of a quoted string.
// If the text is empty, it represents epsilon, the empty string.
type Literal struct {
	Text string
}

func (l Literal) String() string {
	return fmt.Sprintf("%q", l.Text)
}

// Production is a grammar production.
// The identifier and expression must not be nil.
type Production struct {
	Identifier *Identifier
	Expression *Expression
}

func (p Production) String() string {
	return fmt.Sprintf("%s = %v.", p.Identifier, p.Expression)
}

// Term is an expression alternative.
// There must be at least one factor.
// All factors must not be nil.
type Term struct {
	Factors []*Factor
}

func (t Term) String() string {
	ss := make([]string, len(t.Factors))
	for i, f := range t.Factors {
		ss[i] = fmt.Sprint(f)
	}
	return strings.Join(ss, " ")
}

type expectedRuneError struct {
	textError
	expected, actual rune
}

func (e expectedRuneError) Error() string {
	return fmt.Sprintf("%v:%v: expected character %q but found character %q", e.line, e.col, e.expected, e.actual)
}

type expectedTokenError struct {
	textError
	expected, actual token
	text             string
}

func (e expectedTokenError) Error() string {
	return fmt.Sprintf("%v:%v: expected %v but found %v", e.line, e.col, tokenName(e.expected, e.text), tokenValue(e.actual, e.text))
}

type readError textError

func (e readError) Error() string {
	return fmt.Sprintf("%v:%v: cannot read character", e.line, e.col)
}

type textError struct {
	line, col int
}

type unexpectedTokenError struct {
	textError
	token token
	text  string
}

func (e unexpectedTokenError) Error() string {
	return fmt.Sprintf("%v:%v: unexpected %v", e.line, e.col, tokenValue(e.token, e.text))
}

type lexer struct {
	char      rune
	charCol   int
	charLine  int
	chars     []rune
	errs      []error
	token     token
	tokenCol  int
	tokenLine int
	text      string
	reader    io.RuneReader
}

func newLexer(r io.RuneReader) lexer {
	return lexer{
		chars:    make([]rune, 0, 32),
		charLine: 1,
		reader:   r,
	}
}

func (l *lexer) nextChar() {
	var err error
	for {
		l.char, _, err = l.reader.ReadRune()
		if err == io.EOF {
			l.char = 0
			return
		}
		l.charCol++
		if l.char != utf8.RuneError {
			break
		}
		l.errs = append(l.errs, readError{line: l.charLine, col: l.charCol})
	}
	if l.char == '\n' {
		l.charCol = 1
		l.charLine++
	}
}

func (l *lexer) nextToken() {
	if l.char == 0 {
		l.token = other
		return
	}
	l.chars = l.chars[:0]
	l.text = ""
	for unicode.IsSpace(l.char) {
		l.nextChar()
	}
	l.tokenCol = l.charCol
	l.tokenLine = l.charLine
	for 'a' <= l.char && l.char <= 'z' || 'A' <= l.char && l.char <= 'Z' {
		l.chars = append(l.chars, l.char)
		l.nextChar()
	}
	if len(l.chars) > 0 {
		l.token = ident
		l.text = string(l.chars)
		return
	}
	if l.char == '"' {
		l.nextChar()
		for l.char != 0 && l.char != '"' && !unicode.IsSpace(l.char) {
			l.chars = append(l.chars, l.char)
			l.nextChar()
		}
		if l.char != '"' {
			l.errs = append(l.errs, expectedRuneError{
				textError: textError{
					col:  l.charCol,
					line: l.charLine,
				},
				expected: '"',
				actual:   l.char,
			})
		}
		l.nextChar()
		l.token = literal
		l.text = string(l.chars)
		return
	}
	switch l.char {
	case '=':
		l.token = eql
	case '(':
		l.token = lparen
	case ')':
		l.token = rparen
	case '[':
		l.token = lbrak
	case ']':
		l.token = rbrak
	case '{':
		l.token = lbrace
	case '}':
		l.token = rbrace
	case '|':
		l.token = bar
	case '.':
		l.token = period
	default:
		l.token = other
		l.text = string(l.char)
	}
	l.nextChar()
}

type parser struct {
	lexer
	errs []error
}

func newParser(r io.RuneReader) *parser {
	l := newLexer(r)
	l.nextChar()
	l.nextToken()
	return &parser{lexer: l}
}

func (p *parser) expect(t token) {
	if p.token == t {
		p.nextToken()
	} else {
		p.errs = append(p.errs, expectedTokenError{
			textError: textError{
				col:  p.tokenCol,
				line: p.tokenLine,
			},
			actual:   p.token,
			expected: t,
			text:     string(p.text),
		})
	}
}

func (p *parser) grammar() *Grammar {
	if p.token != ident {
		p.errs = append(p.errs, expectedTokenError{
			textError: textError{
				col:  p.tokenCol,
				line: p.tokenLine,
			},
			actual:   p.token,
			expected: ident,
		})
	}
	var ps []*Production
	for p.token == ident {
		ps = append(ps, p.prod())
	}
	return &Grammar{Productions: ps}
}

func (p *parser) prod() *Production {
	prod := &Production{Identifier: &Identifier{Text: p.text}}
	p.nextToken()
	p.expect(eql)
	prod.Expression = p.expr()
	p.expect(period)
	return prod
}

func (p *parser) expr() *Expression {
	ts := []*Term{p.term()}
	for p.token == bar {
		p.nextToken()
		ts = append(ts, p.term())
	}
	return &Expression{Terms: ts}
}

func (p *parser) term() *Term {
	fs := []*Factor{p.factor()}
	for p.token < bar {
		fs = append(fs, p.factor())
	}
	return &Term{Factors: fs}
}

func (p *parser) factor() *Factor {
	var f Factor
	switch p.token {
	case ident:
		f.Identifier = &Identifier{Text: p.text}
		p.nextToken()
	case literal:
		f.Literal = &Literal{Text: p.text}
		p.nextToken()
	case lparen:
		p.nextToken()
		f.Group = p.expr()
		p.expect(rparen)
	case lbrak:
		p.nextToken()
		f.Option = p.expr()
		p.expect(rbrak)
	case lbrace:
		p.nextToken()
		f.Repetition = p.expr()
		p.expect(rbrace)
	default:
		p.errs = append(p.errs, unexpectedTokenError{
			textError: textError{col: p.tokenCol, line: p.tokenLine},
			token:     p.token,
			text:      p.text,
		})
	}
	return &f
}

type token int

const (
	ident   token = 0
	literal token = 2
	lparen  token = 3
	lbrak   token = 4
	lbrace  token = 5
	bar     token = 6
	eql     token = 7
	rparen  token = 8
	rbrak   token = 9
	rbrace  token = 10
	period  token = 11
	other   token = 12
)
