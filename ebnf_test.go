package ebnf

import (
	"testing"

	"github.com/google/go-cmp/cmp"
)

const oberon2 = `
Module        = "MODULE" ident ";" [ImportList] DeclSeq ["BEGIN" StatementSeq] "END" ident ".".
ImportList    = "IMPORT" [ident ":="] ident {"," [ident ":="] ident} ";".
DeclSeq       = { "CONST" {ConstDecl ";" } | "TYPE" {TypeDecl ";"} | "VAR" {VarDecl ";"}} {ProcDecl ";" | ForwardDecl ";"}.
ConstDecl     = IdentDef "=" ConstExpr.
TypeDecl      = IdentDef "=" Type.
VarDecl       = IdentList ":" Type.
ProcDecl      = "PROCEDURE" [Receiver] IdentDef [FormalPars] ";" DeclSeq ["BEGIN" StatementSeq] "END" ident.
ForwardDecl   = "PROCEDURE" "^" [Receiver] IdentDef [FormalPars].
FormalPars    = "(" [FPSection {";" FPSection}] ")" [":" Qualident].
FPSection     = ["VAR"] ident {"," ident} ":" Type.
Receiver      = "(" ["VAR"] ident ":" ident ")".
Type          = Qualident
              | "ARRAY" [ConstExpr {"," ConstExpr}] "OF" Type
              | "RECORD" ["("Qualident")"] FieldList {";" FieldList} "END"
              | "POINTER" "TO" Type
              | "PROCEDURE" [FormalPars].
FieldList     = [IdentList ":" Type].
StatementSeq  = Statement {";" Statement}.
Statement     = [ Designator ":=" Expr
              | Designator ["(" [ExprList] ")"]
              | "IF" Expr "THEN" StatementSeq {"ELSIF" Expr "THEN" StatementSeq} ["ELSE" StatementSeq] "END"
              | "CASE" Expr "OF" Case {"|" Case} ["ELSE" StatementSeq] "END"
              | "WHILE" Expr "DO" StatementSeq "END"
              | "REPEAT" StatementSeq "UNTIL" Expr
              | "FOR" ident ":=" Expr "TO" Expr ["BY" ConstExpr] "DO" StatementSeq "END"
              | "LOOP" StatementSeq "END"
              | "WITH" Guard "DO" StatementSeq {"|" Guard "DO" StatementSeq} ["ELSE" StatementSeq] "END"
              | "EXIT"
              | "RETURN" [Expr]
      ].
Case          = [CaseLabels {"," CaseLabels} ":" StatementSeq].
CaseLabels    = ConstExpr [".." ConstExpr].
Guard         = Qualident ":" Qualident.
ConstExpr     = Expr.
Expr          = SimpleExpr [Relation SimpleExpr].
SimpleExpr    = ["+" | "-"] Term {AddOp Term}.
Term          = Factor {MulOp Factor}.
Factor        = Designator ["(" [ExprList] ")"] | number | character | string | "NIL" | Set | "(" Expr ")" | "~" Factor.
Set           = "{" [Element {"," Element}] "}".
Element       = Expr [".." Expr].
Relation      = "=" | "#" | "<" | "<=" | ">" | ">=" | "IN" | "IS".
AddOp         = "+" | "-" | "OR".
MulOp         = "*" | "/" | "DIV" | "MOD" | "&".
Designator    = Qualident {"." ident | "[" ExprList "]" | "^" | "(" Qualident ")"}.
ExprList      = Expr {"," Expr}.
IdentList     = IdentDef {"," IdentDef}.
Qualident     = [ident "."] ident.
IdentDef      = ident ["*" | "-"].`

func TestParseValid(t *testing.T) {
	for _, test := range []string{
		`S = S .`,
		`S = "S" .`,
		`S = S | S .`,
		`S = (S) .`,
		`S = [S] .`,
		`S = {S} .`,
		oberon2,
	} {
		t.Log(test)
		if _, err := Parse(test); err != nil {
			t.Error(err)
		}
	}
}

func TestParseInvalid(t *testing.T) {
	for _, test := range []string{
		``,
		`S`,
		`S S`,
		`S .`,
		`S =`,
		`S = .`,
		`S = S`,
		`S S .`,
	} {
		t.Log(test)
		if _, err := Parse(test); err == nil {
			t.Error("no error")
		}
	}
}

func TestParseValidateValid(t *testing.T) {
	for _, test := range []string{
		`S = x.`,
		`S = A. A = S.`,
		`S = A. A = B. B = x.`,
		oberon2,
	} {
		t.Log(test)
		g, err := Parse(test)
		if err != nil {
			t.Error("parse error:", err)
			continue
		}
		if err := g.Validate(); err != nil {
			t.Error("validate error:", err)
		}
	}
}

func TestParseValidateInvalid(t *testing.T) {
	for _, test := range []string{
		`s = s.`,
		`S = X.`,
		`S = x. P = x.`,
	} {
		t.Log(test)
		g, err := Parse(test)
		if err != nil {
			t.Error("parse error:", err)
			continue
		}
		if err := g.Validate(); err == nil {
			t.Error("no validate error")
		}
	}
	var g Grammar
	if err := g.Validate(); err == nil {
		t.Error("no validate error")
	}
}

func TestGrammarFirst(t *testing.T) {
	for _, test := range []struct {
		grammar string
		first   map[string]map[any]struct{}
	}{
		{
			`S = x.`,
			map[string]map[any]struct{}{
				"S": {Identifier{Text: "x"}: {}},
			},
		},
		{
			`S = A | B. A = B | C. B = A | C. C = x.`,
			map[string]map[any]struct{}{
				"S": {Identifier{Text: "x"}: {}},
				"A": {Identifier{Text: "x"}: {}},
				"B": {Identifier{Text: "x"}: {}},
				"C": {Identifier{Text: "x"}: {}},
			},
		},
	} {
		t.Log("grammar:", test.grammar)
		g, err := Parse(test.grammar)
		if err != nil {
			t.Error("parse error:", err)
			continue
		}
		if err := g.Validate(); err != nil {
			t.Error("validate error:", err)
			continue
		}
		if a, e := g.First(), test.first; !cmp.Equal(a, e) {
			t.Error("wrong first set:", cmp.Diff(a, e))
		}
	}
}
