package ebnf

import (
	"fmt"
	"io"
)

type parser struct {
	*lexer
	errs []error
}

func newParser(r io.RuneReader) *parser {
	l := newLexer(r)
	l.nextChar()
	l.nextToken()
	return &parser{lexer: l}
}

type expectedTokenError struct {
	lexerError
	expected, actual token
	text             string
}

func (e expectedTokenError) Error() string {
	return fmt.Sprintf("%v:%v: expected %v but found %v", e.line, e.col, tokenString(e.expected, e.text), tokenString(e.actual, e.text))
}

func (p *parser) expect(t token) {
	if p.token == t {
		p.nextToken()
	} else {
		p.errs = append(p.errs, expectedTokenError{
			lexerError: lexerError{
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
	var ps []*Production
	if p.token == tokenIdentifier {
		for p.token == tokenIdentifier {
			ps = append(ps, p.prod())
		}
		if p.token != tokenEOF {
			p.errs = append(p.errs, expectedTokenError{
				lexerError: lexerError{
					col:  p.tokenCol,
					line: p.tokenLine,
				},
				actual:   p.token,
				expected: tokenEOF,
			})
		}
	} else {
		p.errs = append(p.errs, expectedTokenError{
			lexerError: lexerError{
				col:  1,
				line: 1,
			},
			actual:   p.token,
			expected: tokenIdentifier,
		})
	}
	return &Grammar{Productions: ps}
}

func (p *parser) prod() *Production {
	prod := &Production{Identifier: &Identifier{Text: p.text}}
	p.nextToken()
	p.expect(tokenEqual)
	prod.Expression = p.expr()
	p.expect(tokenPeriod)
	return prod
}

func (p *parser) expr() *Expression {
	ts := []*Term{p.term()}
	for p.token == tokenPipe {
		p.nextToken()
		ts = append(ts, p.term())
	}
	return &Expression{Terms: ts}
}

func (p *parser) term() *Term {
	fs := []*Factor{p.factor()}
	for p.token < tokenPipe {
		fs = append(fs, p.factor())
	}
	return &Term{Factors: fs}
}

func (p *parser) factor() *Factor {
	var f Factor
	switch p.token {
	case tokenIdentifier:
		f.Identifier = &Identifier{Text: p.text}
		p.nextToken()
	case tokenLiteral:
		f.Literal = &Literal{Text: p.text}
		p.nextToken()
	case tokenLeftParen:
		p.nextToken()
		f.Group = p.expr()
		p.expect(tokenRightParen)
	case tokenLeftBracket:
		p.nextToken()
		f.Option = p.expr()
		p.expect(tokenRightBracket)
	case tokenLeftBrace:
		p.nextToken()
		f.Repetition = p.expr()
		p.expect(tokenRightBrace)
	default:
		p.errs = append(p.errs, invalidCharError{
			lexerError: lexerError{
				col:  p.tokenCol,
				line: p.tokenLine,
			},
			text:  p.text,
			token: p.token,
		})
	}
	return &f
}
