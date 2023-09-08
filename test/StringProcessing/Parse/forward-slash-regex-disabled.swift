// RUN: %target-typecheck-verify-swift -disable-availability-checking

// REQUIRES: swift_swift_parser

prefix operator /
prefix operator ^/
prefix operator /^/

precedencegroup P {
  associativity: left
}

// The divisions in the body of the below operators make sure we don't try and
// consider them to be ending delimiters of a regex.
infix operator /^/ : P
func /^/ (lhs: Int, rhs: Int) -> Int { 1 / 2 }

infix operator /^ : P
func /^ (lhs: Int, rhs: Int) -> Int { 1 / 2 }

infix operator ^^/ : P
func ^^/ (lhs: Int, rhs: Int) -> Int { 1 / 2 }

_ = #/x/#

_ = /x/
// expected-error@-1 {{'/' is not a prefix unary operator}}
// expected-error@-2 {{cannot find 'x' in scope}}
// expected-error@-3 {{'/' is not a postfix unary operator}}

func baz(_ x: (Int, Int) -> Int, _ y: (Int, Int) -> Int) {}
baz(/, /)
baz(/^, /)
baz(^^/, /)
