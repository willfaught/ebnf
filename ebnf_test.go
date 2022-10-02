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

const textbook = `
EA = TA EB.
EB = "+" TA EB | "".
TA = F TB.
TB = "*" F TB | "".
F = id | "(" EA ")".`

func TestParseValid(t *testing.T) {
	t.Parallel()
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
	t.Parallel()
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
	t.Parallel()
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
	t.Parallel()
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

func TestGrammarFirstNonterminals(t *testing.T) {
	t.Parallel()
	for _, test := range []struct {
		grammar string
		first   map[string]map[any]struct{}
	}{
		{
			`S = a.`,
			map[string]map[any]struct{}{
				"S": {Identifier{"a"}: {}},
			},
		},
		{
			`S = "a".`,
			map[string]map[any]struct{}{
				"S": {Literal{"a"}: {}},
			},
		},
		{
			`S = (a).`,
			map[string]map[any]struct{}{
				"S": {Identifier{"a"}: {}},
			},
		},
		{
			`S = [a].`,
			map[string]map[any]struct{}{
				"S": {
					Identifier{"a"}: {},
					Literal{}:       {},
				},
			},
		},
		{
			`S = {a}.`,
			map[string]map[any]struct{}{
				"S": {
					Identifier{"a"}: {},
					Literal{}:       {},
				},
			},
		},
		{
			`S = a a.`,
			map[string]map[any]struct{}{
				"S": {Identifier{"a"}: {}},
			},
		},
		{
			`S = a b.`,
			map[string]map[any]struct{}{
				"S": {Identifier{"a"}: {}},
			},
		},
		{
			`S = a | a.`,
			map[string]map[any]struct{}{
				"S": {Identifier{"a"}: {}},
			},
		},
		{
			`S = a | b.`,
			map[string]map[any]struct{}{
				"S": {
					Identifier{"a"}: {},
					Identifier{"b"}: {},
				},
			},
		},
		{
			`S = (a b).`,
			map[string]map[any]struct{}{
				"S": {Identifier{"a"}: {}},
			},
		},
		{
			`S = (a | b).`,
			map[string]map[any]struct{}{
				"S": {
					Identifier{"a"}: {},
					Identifier{"b"}: {},
				},
			},
		},
		{
			`S = [a b].`,
			map[string]map[any]struct{}{
				"S": {
					Identifier{"a"}: {},
					Literal{}:       {},
				},
			},
		},
		{
			`S = [a | b].`,
			map[string]map[any]struct{}{
				"S": {
					Identifier{"a"}: {},
					Identifier{"b"}: {},
					Literal{}:       {},
				},
			},
		},
		{
			`S = {a b}.`,
			map[string]map[any]struct{}{
				"S": {
					Identifier{"a"}: {},
					Literal{}:       {},
				},
			},
		},
		{
			`S = {a | b}.`,
			map[string]map[any]struct{}{
				"S": {
					Identifier{"a"}: {},
					Identifier{"b"}: {},
					Literal{}:       {},
				},
			},
		},
		{
			`S = {[(a b) (c d)] [(e f) (g h)]} {[(i j) (k l)] [(m n) (o p)]} | {[(q r) (s t)] [(u v) (w x)]} {[(y z) (aa bb)] [(cc dd) (ee ff)]}.`,
			map[string]map[any]struct{}{
				"S": {
					Identifier{"a"}:  {},
					Identifier{"e"}:  {},
					Identifier{"i"}:  {},
					Identifier{"m"}:  {},
					Identifier{"q"}:  {},
					Identifier{"u"}:  {},
					Identifier{"y"}:  {},
					Identifier{"cc"}: {},
					Literal{}:        {},
				},
			},
		},
		{
			`S = a A. A = b.`,
			map[string]map[any]struct{}{
				"S": {Identifier{"a"}: {}},
				"A": {Identifier{"b"}: {}},
			},
		},
		{
			`S = A B C. A = a. B = "". C = b.`,
			map[string]map[any]struct{}{
				"S": {Identifier{"a"}: {}},
				"A": {Identifier{"a"}: {}},
				"B": {Literal{}: {}},
				"C": {Identifier{"b"}: {}},
			},
		},
		{
			`S = A | B. A = B | C. B = A | C. C = x.`,
			map[string]map[any]struct{}{
				"S": {Identifier{"x"}: {}},
				"A": {Identifier{"x"}: {}},
				"B": {Identifier{"x"}: {}},
				"C": {Identifier{"x"}: {}},
			},
		},
		{
			`S = (a | "") b.`,
			map[string]map[any]struct{}{
				"S": {
					Identifier{"a"}: {},
					Identifier{"b"}: {},
				},
			},
		},
		{
			`S = A b. A = a | "".`,
			map[string]map[any]struct{}{
				"A": {
					Identifier{"a"}: {},
					Literal{}:       {},
				},
				"S": {
					Identifier{"a"}: {},
					Identifier{"b"}: {},
				},
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
		if a, e := g.FirstNonterminals(), test.first; !cmp.Equal(a, e) {
			t.Error("wrong first set:\n", cmp.Diff(a, e))
		}
	}
}

func TestGrammarFollow(t *testing.T) {
	t.Parallel()
	for _, test := range []struct {
		grammar string
		follow  map[string]map[any]struct{}
	}{
		{
			`S = a.`,
			map[string]map[any]struct{}{
				"S": {},
			},
		},
		{
			`S = a S.`,
			map[string]map[any]struct{}{
				"S": {},
			},
		},
		{
			`S = S a.`,
			map[string]map[any]struct{}{
				"S": {Identifier{"a"}: {}},
			},
		},
		{
			`S = S a b.`,
			map[string]map[any]struct{}{
				"S": {Identifier{"a"}: {}},
			},
		},
		{
			`S = c S a b.`,
			map[string]map[any]struct{}{
				"S": {Identifier{"a"}: {}},
			},
		},
		{
			`S = S a | S b.`,
			map[string]map[any]struct{}{
				"S": {Identifier{"a"}: {}, Identifier{"b"}: {}},
			},
		},
		{
			`S = S a S b.`,
			map[string]map[any]struct{}{
				"S": {Identifier{"a"}: {}, Identifier{"b"}: {}},
			},
		},
		{
			`S = S a A b. A = c.`,
			map[string]map[any]struct{}{
				"S": {Identifier{"a"}: {}},
				"A": {Identifier{"b"}: {}},
			},
		},
		{
			`S = A. A = a.`,
			map[string]map[any]struct{}{
				"S": {},
				"A": {},
			},
		},
		{
			`S = A. A = B. B = a.`,
			map[string]map[any]struct{}{
				"S": {},
				"A": {},
				"B": {},
			},
		},
		{
			`S = A a. A = b.`,
			map[string]map[any]struct{}{
				"S": {},
				"A": {Identifier{"a"}: {}},
			},
		},
		{
			`S = A a. A = B b. B = c.`,
			map[string]map[any]struct{}{
				"S": {},
				"A": {Identifier{"a"}: {}},
				"B": {Identifier{"b"}: {}},
			},
		},
		{
			`S = A a. A = B. B = c.`,
			map[string]map[any]struct{}{
				"S": {},
				"A": {Identifier{"a"}: {}},
				"B": {Identifier{"a"}: {}},
			},
		},
		{
			`S = A "" a. A = b.`,
			map[string]map[any]struct{}{
				"S": {},
				"A": {Identifier{"a"}: {}},
			},
		},
		{
			`S = A B a. A = b. B = "".`,
			map[string]map[any]struct{}{
				"S": {},
				"A": {Identifier{"a"}: {}},
				"B": {Identifier{"a"}: {}},
			},
		},
		{
			`S = A B C. A = a. B = "". C = b.`,
			map[string]map[any]struct{}{
				"S": {},
				"A": {Identifier{"b"}: {}},
				"B": {Identifier{"b"}: {}},
				"C": {},
			},
		},
		{
			`S = A B C. A = a. B = "". C = b.`,
			map[string]map[any]struct{}{
				"S": {},
				"A": {Identifier{"b"}: {}},
				"B": {Identifier{"b"}: {}},
				"C": {},
			},
		},
		{
			`S = A A. A = B. B = a.`,
			map[string]map[any]struct{}{
				"S": {},
				"A": {Identifier{"a"}: {}},
				"B": {Identifier{"a"}: {}},
			},
		},
		{
			`S = S a | A. A = A b.`,
			map[string]map[any]struct{}{
				"S": {
					Identifier{"a"}: {},
				},
				"A": {
					Identifier{"a"}: {},
					Identifier{"b"}: {},
				},
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
		if a, e := g.Follow(), test.follow; !cmp.Equal(a, e) {
			t.Error("wrong follow set:\n", cmp.Diff(a, e))
		}
	}
}

func TestGrammarString(t *testing.T) {
	t.Parallel()
	for _, test := range []struct {
		g string
		s string
	}{
		{g: `S = a.`},
		{g: `S = "a".`},
		{g: `S = (a).`},
		{g: `S = [a].`},
		{g: `S = {a}.`},
		{g: `S = a b.`},
		{g: `S = a | b.`},
		{g: `S = (a b).`},
		{g: `S = [a b].`},
		{g: `S = {a b}.`},
		{g: `S = a b | c d.`},
		{g: `S = (a b) (c d).`},
		{g: "S = A.\nA = a."},
	} {
		t.Log("grammar:", test.g)
		g, err := Parse(test.g)
		if err != nil {
			t.Error("parse error:", err)
			continue
		}
		if err := g.Validate(); err != nil {
			t.Error("validate error:", err)
			continue
		}
		e := test.s
		if e == "" {
			e = test.g
		}
		if a := g.String(); !cmp.Equal(a, e) {
			t.Error("wrong string:\n", cmp.Diff(a, e))
		}
	}
}

func TestGrammarConflict(t *testing.T) {
	t.Parallel()
	for _, test := range []struct {
		grammar string
		err     error
	}{
		{`S = a.`, nil},
		{`S = A. A = a.`, nil},
		{`S = a | a.`, FirstFirstConflictError{"S", Identifier{"a"}}},
		{`S = a | b.`, nil},
		{`S = A | A. A = a.`, FirstFirstConflictError{"S", Identifier{"a"}}},
		{`S = A | B. A = a. B = a.`, FirstFirstConflictError{"S", Identifier{"a"}}},
		{`S = A | B. A = a. B = b.`, nil},
		{`S = A B | B. A = a. B = b.`, nil},
		{`S = A B | B. A = a | "". B = b.`, FirstFirstConflictError{"S", Identifier{"b"}}},
		{`S = A a. A = a.`, nil},
		{`S = A "a" "b". A = "a" | "".`, FirstFollowConflictError{"A", Literal{"a"}}},
		{`S = A a. A = b B. B = a | "".`, FirstFollowConflictError{"B", Identifier{"a"}}},
		{textbook, nil},
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
		if a, e := g.Conflict(), test.err; !cmp.Equal(a, e) {
			t.Error("wrong conflict:\n", cmp.Diff(a, e))
		}
	}
}
