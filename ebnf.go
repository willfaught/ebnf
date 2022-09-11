// Package ebnf represents and parses a variant of [Extended Backus-Naur Form] called [Wirth Syntax Notation].
// Terminal identifiers must begin with a lowercase letter.
// Non-terminal identifiers must begin with an uppercase letter.
// The first production defines the start non-terminal identifier.
// Terminal identifiers are assumed to be defined elsewhere.
//
// [Extended Backus-Naur Form]: https://en.wikipedia.org/wiki/Extended_Backus–Naur_form
// [Wirth Syntax Notation]: https://en.wikipedia.org/wiki/Wirth_syntax_notation
package ebnf

import (
	"bytes"
	"errors"
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
type Expression struct {
	Terms []*Term
}

// Factor is a concrete form that can be sequenced.
type Factor struct {
	Group      *Expression
	Identifier *Identifier
	Literal    *Literal
	Option     *Expression
	Repetition *Expression
}

// Grammar is an abstract syntax tree for a grammar.
type Grammar struct {
	Productions []*Production
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

// Validate checks that production identifiers are capitalized and defined.
func (g *Grammar) Validate() error {
	var errs []error
	used := map[string]bool{}
	traverse(g, func(item any) {
		switch item := item.(type) {
		case *Expression:
			if item == nil {
				errs = append(errs, errors.New("expression is nil"))
				return
			}
			if len(item.Terms) == 0 {
				errs = append(errs, errors.New("expression is empty"))
				return
			}
		case *Factor:
			if item == nil {
				errs = append(errs, errors.New("factor is nil"))
				return
			}
			if *item == (Factor{}) {
				errs = append(errs, errors.New("factor is empty"))
				return
			}
		case *Grammar:
			if item == nil {
				errs = append(errs, errors.New("grammar is nil"))
				return
			}
			if len(item.Productions) == 0 {
				errs = append(errs, errors.New("grammar is empty"))
				return
			}
			for _, p := range item.Productions {
				if p.Identifier != nil && len(p.Identifier.Text) > 0 {
					if r, _ := utf8.DecodeRuneInString(p.Identifier.Text); unicode.IsUpper(r) {
						if _, ok := used[p.Identifier.Text]; ok {
							errs = append(errs, fmt.Errorf("identifier %q is defined twice", p.Identifier.Text))
							continue
						}
						used[p.Identifier.Text] = false
					}
				}
			}
		case *Identifier:
			if item == nil {
				errs = append(errs, errors.New("identifier is nil"))
				return
			}
			if len(item.Text) == 0 {
				errs = append(errs, errors.New("identifier is empty"))
				return
			}
			r, n := utf8.DecodeRuneInString(item.Text)
			if r == utf8.RuneError || n == 0 {
				errs = append(errs, errors.New("identifier is invalid"))
				return
			}
			if unicode.IsUpper(r) {
				b, ok := used[item.Text]
				if !ok {
					errs = append(errs, fmt.Errorf("identifier %q is undefined", item.Text))
					return
				}
				if !b {
					used[item.Text] = true
				}
			}
		case *Literal:
			if item == nil {
				errs = append(errs, errors.New("literal is nil"))
				return
			}
		case *Production:
			if item == nil {
				errs = append(errs, errors.New("production is nil"))
				return
			}
			if item.Identifier == nil {
				errs = append(errs, errors.New("identifier is nil"))
				return
			}
			if len(item.Identifier.Text) == 0 {
				errs = append(errs, errors.New("identifier is empty"))
				return
			}
			r, n := utf8.DecodeRuneInString(item.Identifier.Text)
			if r == utf8.RuneError || n == 0 {
				errs = append(errs, errors.New("identifier is invalid"))
				return
			}
			if !unicode.IsUpper(r) {
				errs = append(errs, fmt.Errorf("identifier %q starts with a lowercase character", item.Identifier.Text))
				return
			}
		case *Term:
			if item == nil {
				errs = append(errs, errors.New("term is nil"))
				return
			}
			if len(item.Factors) == 0 {
				errs = append(errs, errors.New("term is empty"))
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

// func (g *Grammar) terminals() []string {
// }

// func (g *Grammar) nonTerminals() []string {
// }

// func firstTerminals(item any, first map[string]struct{}) {
// }

// func (g *Grammar) followTerminals(ident string) []string {
// }

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

// Identifier is a terminal or non-terminal identifier.
type Identifier struct {
	Text string
}

// Literal is the content of a quoted string.
type Literal struct {
	Text string
}

// Production is a production.
type Production struct {
	Identifier *Identifier
	Expression *Expression
}

// Term is an alternative.
type Term struct {
	Factors []*Factor
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
