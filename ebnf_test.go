package ebnf

import "testing"

func TestParseValid(t *testing.T) {
	for _, test := range []string{
		`S = S .`,
		`S = "S" .`,
		`S = S | S .`,
		`S = (S) .`,
		`S = [S] .`,
		`S = {S} .`,
	} {
		t.Log(test)
		if _, err := Parse(test); err != nil {
			t.Error(err)
		}
	}
}

func TestParseInvalid(t *testing.T) {
	for _, test := range []string{
		`S = S`, // TODO: But is error correct?
	} {
		t.Log(test)
		if _, err := Parse(test); err == nil {
			t.Error("no error")
		}
	}
}

// TODO: More testing
