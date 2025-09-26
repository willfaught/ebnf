package ebnf

import (
	"fmt"
	"io"
	"unicode"
	"unicode/utf8"
)

type expectedRuneError struct {
	textError
	expected, actual rune
}

func (e expectedRuneError) Error() string {
	return fmt.Sprintf("%v:%v: expected character %q but found character %q", e.line, e.col, e.expected, e.actual)
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

type expectedTokenError struct {
	textError
	expected, actual token
	text             string
}

func (e expectedTokenError) Error() string {
	return fmt.Sprintf("%v:%v: expected %v but found %v", e.line, e.col, tokenName(e.expected, e.text), tokenValue(e.actual, e.text))
}

type unexpectedTokenError struct {
	textError
	token token
	text  string
}

func (e unexpectedTokenError) Error() string {
	return fmt.Sprintf("%v:%v: unexpected %v", e.line, e.col, tokenValue(e.token, e.text))
}

type readError textError

func (e readError) Error() string {
	return fmt.Sprintf("%v:%v: cannot read character", e.line, e.col)
}

type textError struct {
	line, col int
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

func newLexer(r io.RuneReader) *lexer {
	return &lexer{
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
