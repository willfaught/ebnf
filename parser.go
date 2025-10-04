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
			actual:   p.token,
			expected: t,
			lexerError: lexerError{
				col:  p.tokenCol,
				line: p.tokenLine,
			},
			text: string(p.text),
		})
	}
}

type unexpectedTokenError struct {
	lexerError
	token token
	text  string
}

func (e unexpectedTokenError) Error() string {
	return fmt.Sprintf("%v:%v: unexpected %v", e.line, e.col, tokenString(e.token, e.text))
}

func (p *parser) parseFactor() *Factor {
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
		f.Group = p.parseExpression()
		p.expect(tokenRightParen)
	case tokenLeftBracket:
		p.nextToken()
		f.Option = p.parseExpression()
		p.expect(tokenRightBracket)
	case tokenLeftBrace:
		p.nextToken()
		f.Repetition = p.parseExpression()
		p.expect(tokenRightBrace)
	default:
		p.errs = append(p.errs, unexpectedTokenError{
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

func (p *parser) parseTerm() *Term {
	fs := []*Factor{p.parseFactor()}
	for tokenInvalid < p.token && p.token < tokenPipe {
		fs = append(fs, p.parseFactor())
	}
	return &Term{Factors: fs}
}

func (p *parser) parseExpression() *Expression {
	ts := []*Term{p.parseTerm()}
	for p.token == tokenPipe {
		p.nextToken()
		ts = append(ts, p.parseTerm())
	}
	return &Expression{Terms: ts}
}

func (p *parser) parseProduction() *Production {
	prod := &Production{Identifier: &Identifier{Text: p.text}}
	p.nextToken()
	p.expect(tokenEqual)
	prod.Expression = p.parseExpression()
	p.expect(tokenPeriod)
	return prod
}

func (p *parser) parseGrammar() *Grammar {
	var ps []*Production
	if p.token == tokenIdentifier {
		for p.token == tokenIdentifier {
			ps = append(ps, p.parseProduction())
		}
		if p.token != tokenEOF {
			p.errs = append(p.errs, expectedTokenError{
				actual:   p.token,
				expected: tokenEOF,
				lexerError: lexerError{
					col:  p.tokenCol,
					line: p.tokenLine,
				},
			})
		}
	} else {
		p.errs = append(p.errs, expectedTokenError{
			actual:   p.token,
			expected: tokenIdentifier,
			lexerError: lexerError{
				col:  1,
				line: 1,
			},
		})
	}
	return &Grammar{Productions: ps}
}
