package ebnf

import "fmt"

type token int

const (
	tokenInvalid token = 0
	tokenIdent   token = 1
	tokenLiteral token = 2
	tokenLparen  token = 3
	tokenLbrak   token = 4
	tokenLbrace  token = 5
	tokenBar     token = 6
	tokenEql     token = 7
	tokenRparen  token = 8
	tokenRbrak   token = 9
	tokenRbrace  token = 10
	tokenPeriod  token = 11
	tokenEOF     token = 12
)

func tokenString(t token, text string) string {
	var s string
	switch t {
	case tokenInvalid:
		if text == "" {
			s = "invalid syntax"
		} else {
			s = fmt.Sprintf("invalid syntax %q", text)
		}
	case tokenIdent:
		if text == "" {
			s = "identifier"
		} else {
			s = fmt.Sprintf("identifier %q", text)
		}
	case tokenLiteral:
		if text == "" {
			s = "literal"
		} else {
			s = fmt.Sprintf("literal %q", text)
		}
	case tokenLparen:
		s = "("
	case tokenLbrak:
		s = "["
	case tokenLbrace:
		s = "{"
	case tokenBar:
		s = "|"
	case tokenEql:
		s = "="
	case tokenRparen:
		s = ")"
	case tokenRbrak:
		s = "]"
	case tokenRbrace:
		s = "}"
	case tokenPeriod:
		s = "."
	case tokenEOF:
		s = "end of file"
	default:
		panic(t)
	}
	return s
}
