package ebnf

import "fmt"

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
