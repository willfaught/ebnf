package ebnf

import (
	"fmt"
	"io"
	"unicode"
	"unicode/utf8"
)

type expectedTokenError struct {
	textError
	expected, actual token
	text             string
}

func (e expectedTokenError) Error() string {
	return fmt.Sprintf("%v:%v: expected %v but found %v", e.line, e.col, tokenString(e.expected, e.text), tokenString(e.actual, e.text))
}

type invalidCharError struct {
	textError
	token token
	text  string
}

func (e invalidCharError) Error() string {
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
		l.errs = append(l.errs, invalidCharError{
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
