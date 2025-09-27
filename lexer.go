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
	return fmt.Sprintf("%v:%v: expected %q but found %q", e.line, e.col, e.expected, e.actual)
}

type token int

const (
	invalid token = 0
	ident   token = 1
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
	eof     token = 12
)

func tokenString(t token, text string) string {
	var s string
	switch t {
	case invalid:
		if text == "" {
			s = "invalid syntax"
		} else {
			s = fmt.Sprintf("invalid syntax %q", text)
		}
	case ident:
		if text == "" {
			s = "identifier"
		} else {
			s = fmt.Sprintf("identifier %q", text)
		}
	case literal:
		if text == "" {
			s = "literal"
		} else {
			s = fmt.Sprintf("literal %q", text)
		}
	case lparen:
		s = "("
	case lbrak:
		s = "["
	case lbrace:
		s = "{"
	case bar:
		s = "|"
	case eql:
		s = "="
	case rparen:
		s = ")"
	case rbrak:
		s = "]"
	case rbrace:
		s = "}"
	case period:
		s = "."
	case eof:
		s = "end of file"
	default:
		panic(t)
	}
	return s
}

type expectedTokenError struct {
	textError
	expected, actual token
	text             string
}

func (e expectedTokenError) Error() string {
	return fmt.Sprintf("%v:%v: expected %v but found %v", e.line, e.col, tokenString(e.expected, e.text), tokenString(e.actual, e.text))
}

type unexpectedTokenError struct {
	textError
	token token
	text  string
}

func (e unexpectedTokenError) Error() string {
	return fmt.Sprintf("%v:%v: unexpected %v", e.line, e.col, tokenString(e.token, e.text))
}

type readError struct {
	textError
	err error
}

func (e readError) Error() string {
	return fmt.Sprintf("%v:%v: cannot read character: %v", e.line, e.col, e.err)
}

type unexpectedEOFError struct {
	textError
	expected rune
}

func (e unexpectedEOFError) Error() string {
	return fmt.Sprintf("%v:%v: expected %q but found end of file", e.line, e.col, e.expected)
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

const eot = -1

func (l *lexer) nextChar() {
	if l.char == eot {
		return
	}
	var err error
	l.char, _, err = l.reader.ReadRune()
	if err != nil {
		if err != io.EOF {
			l.errs = append(l.errs, readError{
				err: err,
				textError: textError{
					col:  l.charCol,
					line: l.charLine,
				},
			})
		}
		l.char = eot
		return
	}
	l.charCol++
	if l.char == utf8.RuneError {
		l.errs = append(l.errs, readError{
			err: fmt.Errorf("invalid utf-8 character"),
			textError: textError{
				col:  l.charCol,
				line: l.charLine,
			},
		})
		l.char = eot
		return
	}
	if l.char == 0 {
		l.errs = append(l.errs, readError{
			err: fmt.Errorf("invalid null character"),
			textError: textError{
				col:  l.charCol,
				line: l.charLine,
			},
		})
		l.char = eot
		return
	}
	if l.char == '\n' {
		l.charCol = 1
		l.charLine++
	}
}

func (l *lexer) nextToken() {
	if l.token == eof {
		return
	}
	if l.char == eot {
		l.token = eof
		return
	}
	l.chars = l.chars[:0]
	l.text = ""
	for unicode.IsSpace(l.char) {
		l.nextChar()
	}
	l.tokenCol = l.charCol
	l.tokenLine = l.charLine
	if l.char == eot {
		l.token = eof
		return
	}
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
		for l.char != eot && l.char != '"' {
			l.chars = append(l.chars, l.char)
			l.nextChar()
		}
		l.text = string(l.chars)
		if l.char == eot {
			l.token = invalid
			l.errs = append(l.errs, unexpectedEOFError{
				textError: textError{
					col:  l.charCol,
					line: l.charLine,
				},
				expected: '"',
			})
		} else {
			l.token = literal
			l.nextChar()
		}
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
		l.token = invalid
		l.text = string(l.char)
		l.errs = append(l.errs, unexpectedTokenError{
			textError: textError{
				col:  l.charCol,
				line: l.charLine,
			},
			token: l.token,
			text:  l.text,
		})
	}
	l.nextChar()
}
