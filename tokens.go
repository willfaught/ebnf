package ebnf

import "fmt"

type token int

const (
	tokenInvalid token = iota
	tokenEnd
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
)

func tokenString(t token, text string) string {
	var s string
	switch t {
	case tokenInvalid:
		if text == "" {
			s = "invalid token"
		} else {
			s = fmt.Sprintf("invalid token %q", text)
		}
	case tokenIdentifier:
		if text == "" {
			s = "identifier token"
		} else {
			s = fmt.Sprintf("identifier token %q", text)
		}
	case tokenLiteral:
		if text == "" {
			s = "literal token"
		} else {
			s = fmt.Sprintf("literal token %q", text)
		}
	case tokenLeftParen:
		s = `token "("`
	case tokenLeftBracket:
		s = `token "["`
	case tokenLeftBrace:
		s = `token "{"`
	case tokenPipe:
		s = `token "|"`
	case tokenEqual:
		s = `token "="`
	case tokenRightParen:
		s = `token ")"`
	case tokenRightBracket:
		s = `token "]"`
	case tokenRightBrace:
		s = `token "}"`
	case tokenPeriod:
		s = `token "."`
	case tokenEnd:
		s = "end of tokens"
	default:
		panic(t)
	}
	return s
}
