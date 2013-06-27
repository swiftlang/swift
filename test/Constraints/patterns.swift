// RUN: %swift -parse -verify %s

// Raise an error if pattern productions are used in expressions.
var a = _ // expected-error{{'_' pattern cannot appear in an expression}}
var b = var a // expected-error{{'var' binding pattern cannot appear in an expression}}
var c = is Int // expected-error{{prefix 'is' pattern cannot appear in an expression}}

// TODO: Bad recovery in these cases. Although patterns are never valid
// expr-unary productions, it would be good to parse them anyway for recovery.
//var d = _ + 1
//var e = 2 + var y
//var e = var y + 2
