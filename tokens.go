package ebnf

import "fmt"

type token int

const (
	tokenInvalid token = iota
	tokenIdentifier
	tokenLiteral
	tokenLeftParen
	tokenLeftBracket
	tokenLeftBrace
	tokenPipe
	tokenEqual
	tokenRightParen
	tokenRightBracket
	tokenRightBrace
	tokenPeriod
	tokenEOF
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
	case tokenIdentifier:
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
	case tokenLeftParen:
		s = `"("`
	case tokenLeftBracket:
		s = `"["`
	case tokenLeftBrace:
		s = `"{"`
	case tokenPipe:
		s = `"|"`
	case tokenEqual:
		s = `"="`
	case tokenRightParen:
		s = `")"`
	case tokenRightBracket:
		s = `"]"`
	case tokenRightBrace:
		s = `"}"`
	case tokenPeriod:
		s = `"."`
	case tokenEOF:
		s = "end of file"
	default:
		panic(t)
	}
	return s
}
