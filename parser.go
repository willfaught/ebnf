package ebnf

import "io"

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
	var ps []*Production
	if p.token == ident {
		for p.token == ident {
			ps = append(ps, p.prod())
		}
		if p.token != eof {
			p.errs = append(p.errs, expectedTokenError{
				textError: textError{
					col:  p.tokenCol,
					line: p.tokenLine,
				},
				actual:   p.token,
				expected: eof,
			})
		}
	} else {
		p.errs = append(p.errs, expectedTokenError{
			textError: textError{
				col:  1,
				line: 1,
			},
			actual:   p.token,
			expected: ident,
		})
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
			textError: textError{
				col:  p.tokenCol,
				line: p.tokenLine,
			},
			text:  p.text,
			token: p.token,
		})
	}
	return &f
}
