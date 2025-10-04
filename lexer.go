package ebnf

import (
	"fmt"
	"io"
	"unicode"
	"unicode/utf8"
)

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

type lexerError struct {
	line, col int
}

type readError struct {
	lexerError
	err error
}

func (e readError) Error() string {
	return fmt.Sprintf("%v:%v: cannot read character: %v", e.line, e.col, e.err)
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
				lexerError: lexerError{
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
			lexerError: lexerError{
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
			lexerError: lexerError{
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

type invalidTokenError struct {
	lexerError
	text string
}

func (e invalidTokenError) Error() string {
	return fmt.Sprintf("%v:%v: invalid token %q", e.line, e.col, e.text)
}

type unexpectedEOFError struct {
	lexerError
	expected rune
}

func (e unexpectedEOFError) Error() string {
	return fmt.Sprintf("%v:%v: expected %q but found end of file", e.line, e.col, e.expected)
}

func (l *lexer) nextToken() {
	if l.token == tokenEOF {
		return
	}
	if l.char == eot {
		l.token = tokenEOF
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
		l.token = tokenEOF
		return
	}
	for 'a' <= l.char && l.char <= 'z' || 'A' <= l.char && l.char <= 'Z' {
		l.chars = append(l.chars, l.char)
		l.nextChar()
	}
	if len(l.chars) > 0 {
		l.token = tokenIdentifier
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
			l.token = tokenInvalid
			l.errs = append(l.errs, unexpectedEOFError{
				expected: '"',
				lexerError: lexerError{
					col:  l.charCol,
					line: l.charLine,
				},
			})
		} else {
			l.token = tokenLiteral
			l.nextChar()
		}
		return
	}
	switch l.char {
	case '=':
		l.token = tokenEqual
	case '(':
		l.token = tokenLeftParen
	case ')':
		l.token = tokenRightParen
	case '[':
		l.token = tokenLeftBracket
	case ']':
		l.token = tokenRightBracket
	case '{':
		l.token = tokenLeftBrace
	case '}':
		l.token = tokenRightBrace
	case '|':
		l.token = tokenPipe
	case '.':
		l.token = tokenPeriod
	default:
		l.token = tokenInvalid
		l.text = string(l.char)
		l.errs = append(l.errs, invalidTokenError{
			lexerError: lexerError{
				col:  l.charCol,
				line: l.charLine,
			},
			text: l.text,
		})
	}
	l.nextChar()
}
