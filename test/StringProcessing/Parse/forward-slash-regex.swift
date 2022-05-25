// RUN: %target-typecheck-verify-swift -enable-bare-slash-regex -disable-availability-checking
// REQUIRES: swift_in_compiler
// REQUIRES: concurrency

prefix operator /
prefix operator ^/
prefix operator /^/

prefix func ^/ <T> (_ x: T) -> T { x } // expected-note {{'^/' declared here}}

prefix operator !!
prefix func !! <T>(_ x: T) -> T { x }

prefix operator ^^
prefix func ^^ <T>(_ x: T) -> T { x }

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

let i = 0 /^/ 1/^/3

let x = /abc/
_ = /abc/
_ = /x/.self
_ = /\//
_ = /\\/

// This is just here to appease typo correction.
let y = 0

// These unfortunately become prefix `=` and infix `=/` respectively. We could
// likely improve the diagnostic though.
do {
  let z=/0/
  // expected-error@-1 {{type annotation missing in pattern}}
  // expected-error@-2 {{consecutive statements on a line must be separated by ';'}}
  // expected-error@-3 {{expected expression}}
}
do {
  _=/0/
  // expected-error@-1 {{'_' can only appear in a pattern or on the left side of an assignment}}
  // expected-error@-2 {{cannot find operator '=/' in scope}}
  // expected-error@-3 {{'/' is not a postfix unary operator}}
}

// No closing '/' so a prefix operator.
_ = /x
// expected-error@-1 {{'/' is not a prefix unary operator}}

_ = !/x/
// expected-error@-1 {{cannot convert value of type 'Regex<Substring>' to expected argument type 'Bool'}}

_ = (!/x/)
// expected-error@-1 {{cannot convert value of type 'Regex<Substring>' to expected argument type 'Bool'}}

_ = !/ /
// expected-error@-1 {{regex literal may not start with space; add backslash to escape}}
// expected-error@-2 {{cannot convert value of type 'Regex<Substring>' to expected argument type 'Bool'}}

_ = !!/ /
// expected-error@-1 {{regex literal may not start with space; add backslash to escape}}

_ = !!/x/
_ = (!!/x/)

_ = /^)
// expected-error@-1 {{unterminated regex literal}}
// expected-error@-2 {{closing ')' does not balance any groups openings}}

_ = /x/! // expected-error {{cannot force unwrap value of non-optional type 'Regex<Substring>'}}
_ = /x/ + /y/ // expected-error {{binary operator '+' cannot be applied to two 'Regex<Substring>' operands}}

_ = /x/+/y/
// expected-error@-1 {{cannot find operator '+/' in scope}}
// expected-error@-2 {{'/' is not a postfix unary operator}}

_ = /x/?.blah
// expected-error@-1 {{cannot use optional chaining on non-optional value of type 'Regex<Substring>'}}
// expected-error@-2 {{value of type 'Regex<Substring>' has no member 'blah'}}
_ = /x/!.blah
// expected-error@-1 {{cannot force unwrap value of non-optional type 'Regex<Substring>'}}
// expected-error@-2 {{value of type 'Regex<Substring>' has no member 'blah'}}

_ = /x /? // expected-error {{cannot use optional chaining on non-optional value of type 'Regex<Substring>'}}
  .blah // expected-error {{value of type 'Regex<Substring>' has no member 'blah'}}

_ = 0; /x / // expected-warning {{regular expression literal is unused}}

_ = /x / ? 0 : 1 // expected-error {{cannot convert value of type 'Regex<Substring>' to expected condition type 'Bool'}}
_ = .random() ? /x / : .blah // expected-error {{type 'Regex<Substring>' has no member 'blah'}}

_ = /x/ ?? /x/ // expected-warning {{left side of nil coalescing operator '??' has non-optional type 'Regex<Substring>', so the right side is never used}}
_ = /x / ?? /x / // expected-warning {{left side of nil coalescing operator '??' has non-optional type 'Regex<Substring>', so the right side is never used}}

_ = /x/??/x/ // expected-error {{'/' is not a postfix unary operator}}

_ = /x/ ... /y/ // expected-error {{referencing operator function '...' on 'Comparable' requires that 'Regex<Substring>' conform to 'Comparable'}}

_ = /x/.../y/
// expected-error@-1 {{missing whitespace between '...' and '/' operators}}
// expected-error@-2 {{'/' is not a postfix unary operator}}

_ = /x /...
// expected-error@-1 {{unary operator '...' cannot be applied to an operand of type 'Regex<Substring>'}}
// expected-note@-2 {{overloads for '...' exist with these partially matching parameter lists}}

do {
  _ = true / false /; // expected-error {{expected expression after operator}}
}

_ = "\(/x/)"

func defaulted(x: Regex<Substring> = /x/) {}

func foo<T>(_ x: T, y: T) {}
foo(/abc/, y: /abc /)

func bar<T>(_ x: inout T) {}
bar(&/x/) // expected-error {{cannot pass immutable value as inout argument: literals are not mutable}}

struct S {
  subscript(x: Regex<Substring>) -> Void { () }
}

func testSubscript(_ x: S) {
  x[/x/]
  x[/x /]
}

func testReturn() -> Regex<Substring> {
  if .random() {
    return /x/
  }
  return /x /
}

func testThrow() throws {
  throw /x / // expected-error {{thrown expression type 'Regex<Substring>' does not conform to 'Error'}}
}

_ = [/abc/, /abc /]
_ = [/abc/:/abc/] // expected-error {{generic struct 'Dictionary' requires that 'Regex<Substring>' conform to 'Hashable'}}
_ = [/abc/ : /abc/] // expected-error {{generic struct 'Dictionary' requires that 'Regex<Substring>' conform to 'Hashable'}}
_ = [/abc /:/abc /] // expected-error {{generic struct 'Dictionary' requires that 'Regex<Substring>' conform to 'Hashable'}}
_ = [/abc /: /abc /] // expected-error {{generic struct 'Dictionary' requires that 'Regex<Substring>' conform to 'Hashable'}}
_ = (/abc/, /abc /)
_ = ((/abc /))

_ = { /abc/ }
_ = {
  /abc/
}

let _: () -> Int = {
  0
  / 1 /
  2
}

_ = {
  0 // expected-warning {{integer literal is unused}}
  /1 / // expected-warning {{regular expression literal is unused}}
  2 // expected-warning {{integer literal is unused}}
}

// Operator chain, as a regex literal may not start with space.
_ = 2
/ 1 / .bitWidth

_ = 2
/1/ .bitWidth // expected-error {{value of type 'Regex<Substring>' has no member 'bitWidth'}}

_ = 2
/ 1 /
  .bitWidth

_ = 2
/1 /
  .bitWidth // expected-error {{value of type 'Regex<Substring>' has no member 'bitWidth'}}

let z =
/y/

// While '.' is technically an operator character, it seems more likely that
// the user hasn't written the member name yet.
_ = 0. / 1 / 2 // expected-error {{expected member name following '.'}}
_ = 0 . / 1 / 2 // expected-error {{expected member name following '.'}}

switch "" {
case /x/:
  break
case _ where /x /:
  // expected-error@-1 {{cannot convert value of type 'Regex<Substring>' to expected condition type 'Bool'}}
  break
default:
  break
}

do {} catch /x / {}
// expected-error@-1 {{expression pattern of type 'Regex<Substring>' cannot match values of type 'any Error'}}
// expected-error@-2 {{binary operator '~=' cannot be applied to two 'any Error' operands}}
// expected-warning@-3 {{'catch' block is unreachable because no errors are thrown in 'do' block}}

switch /x / {
default:
  break
}

if /x / {} // expected-error {{cannot convert value of type 'Regex<Substring>' to expected condition type 'Bool'}}
if /x /.smth {} // expected-error {{value of type 'Regex<Substring>' has no member 'smth'}}

func testGuard() {
  guard /x/ else { return } // expected-error {{cannot convert value of type 'Regex<Substring>' to expected condition type 'Bool'}}
}

for x in [0] where /x/ {} // expected-error {{cannot convert value of type 'Regex<Substring>' to expected condition type 'Bool'}}

typealias Magic<T> = T
_ = /x/ as Magic
_ = /x/ as! String // expected-warning {{cast from 'Regex<Substring>' to unrelated type 'String' always fails}}

_ = type(of: /x /)

do {
  let /x / // expected-error {{expected pattern}}
}

_ = try /x/; _ = try /x /
// expected-warning@-1 2{{no calls to throwing functions occur within 'try' expression}}
_ = try? /x/; _ = try? /x /
// expected-warning@-1 2{{no calls to throwing functions occur within 'try' expression}}
_ = try! /x/; _ = try! /x /
// expected-warning@-1 2{{no calls to throwing functions occur within 'try' expression}}

_ = await /x / // expected-warning {{no 'async' operations occur within 'await' expression}}

/x/ = 0 // expected-error {{cannot assign to value: literals are not mutable}}
/x/() // expected-error {{cannot call value of non-function type 'Regex<Substring>'}}

// We treat the following as comments, as it seems more likely the user has
// written a comment and is still in the middle of writing the characters before
// it.
_ = /x// comment
// expected-error@-1 {{'/' is not a prefix unary operator}}

_ = /x // comment
// expected-error@-1 {{'/' is not a prefix unary operator}}

_ = /x/*comment*/
// expected-error@-1 {{'/' is not a prefix unary operator}}

// MARK: Unapplied operators

// These become regex literals, unless surrounded in parens.
func baz(_ x: (Int, Int) -> Int, _ y: (Int, Int) -> Int) {} // expected-note 4{{'baz' declared here}}
baz(/, /)
// expected-error@-1 {{cannot convert value of type 'Regex<Substring>' to expected argument type '(Int, Int) -> Int'}}
// expected-error@-2 {{missing argument for parameter #2 in call}}
baz(/,/)
// expected-error@-1 {{cannot convert value of type 'Regex<Substring>' to expected argument type '(Int, Int) -> Int'}}
// expected-error@-2 {{missing argument for parameter #2 in call}}
baz((/), /)

baz(/^, /)
// expected-error@-1 {{cannot convert value of type 'Regex<Substring>' to expected argument type '(Int, Int) -> Int'}}
// expected-error@-2 {{missing argument for parameter #2 in call}}

baz((/^), /)

baz(^^/, /) // expected-error {{missing argument for parameter #2 in call}}
baz((^^/), /)

func bazbaz(_ x: (Int, Int) -> Int, _ y: Int) {}
bazbaz(/, 0)
bazbaz(^^/, 0)

func qux<T>(_ x: (Int, Int) -> Int, _ y: T) -> Int { 0 }
_ = qux(/, 1) / 2
do {
  _ = qux(/, "(") / 2
  // expected-error@-1 {{cannot convert value of type 'Regex<(Substring, Substring)>' to expected argument type '(Int, Int) -> Int'}}
  // expected-error@-2:21 {{expected ',' separator}}
}
_ = qux((/), "(") / 2
_ = qux(/, 1) // this comment tests to make sure we don't try and end the regex on the starting '/' of '//'.
_ = qux(/, 1) /* same thing with a block comment */

@discardableResult
func quxqux(_ x: (Int, Int) -> Int) -> Int { 0 }
quxqux(/^/) // expected-error {{cannot convert value of type 'Regex<Substring>' to expected argument type '(Int, Int) -> Int'}}
quxqux((/^/)) // expected-error {{cannot convert value of type 'Regex<Substring>' to expected argument type '(Int, Int) -> Int'}}
quxqux({ $0 /^/ $1 })

quxqux(!/^/)
// expected-error@-1 {{cannot convert value of type 'Bool' to expected argument type '(Int, Int) -> Int'}}
// expected-error@-2 {{cannot convert value of type 'Regex<Substring>' to expected argument type 'Bool'}}

quxqux(/^)
_ = quxqux(/^) / 1

let arr: [Double] = [2, 3, 4]
_ = arr.reduce(1, /) / 3
_ = arr.reduce(1, /) + arr.reduce(1, /)

// MARK: ')' disambiguation behavior

_ = (/x)
// expected-error@-1 {{'/' is not a prefix unary operator}}

_ = (/x)/
// expected-error@-1 {{'/' is not a prefix unary operator}}
// expected-error@-2 {{'/' is not a postfix unary operator}}

_ = (/[(0)])/
// expected-error@-1 {{'/' is not a prefix unary operator}}
// expected-error@-2 {{'/' is not a postfix unary operator}}

_ = /[(0)]/
_ = /(x)/
_ = /[)]/
_ = /[a\])]/
_ = /([)])/
_ = /]]][)]/

_ = /
// expected-error@-1 {{unterminated regex literal}}

_ = /)
// expected-error@-1 {{unterminated regex literal}}
// expected-error@-2 {{closing ')' does not balance any groups openings}}

let fn: (Int, Int) -> Int = (/)

_ = /\()/
// expected-error@-1 {{'/' is not a prefix unary operator}}
// expected-error@-2 {{'/' is not a postfix unary operator}}
// expected-error@-3 {{invalid component of Swift key path}}

do {
  let _: Regex = (/whatever\)/
  // expected-note@-1 {{to match this opening '('}}
} // expected-error {{expected ')' in expression list}}
do {
  _ = /(()()))/
  // expected-error@-1 {{'/' is not a prefix unary operator}}
  // expected-error@-2 {{consecutive statements on a line must be separated by ';'}}
  // expected-error@-3 {{expected expression}}
}
do {
  _ = /[x])/
  // expected-error@-1 {{'/' is not a prefix unary operator}}
  // expected-error@-2 {{consecutive statements on a line must be separated by ';'}}
  // expected-error@-3 {{expected expression}}
}
do {
  _ = /[\]])/
  // expected-error@-1 {{expected expression path in Swift key path}}
}

_ = ^/x/
// expected-error@-1 {{'^' is not a prefix unary operator}}

_ = (^/x)/
// expected-error@-1 {{'/' is not a postfix unary operator}}

_ = (!!/x/)

_ = ^/"/"
// expected-error@-1 {{'^' is not a prefix unary operator}}
// expected-error@-2 {{unterminated string literal}}

_ = ^/"[/"
// expected-error@-1 {{'^' is not a prefix unary operator}}
// expected-error@-2 {{unterminated string literal}}
// expected-error@-3 {{expected custom character class members}}

_ = (^/)("/")

// MARK: Starting characters

// Fine.
_ = /./

// You need to escape if you want a regex literal to start with these characters.
_ = /\ /
_ = / / // expected-error {{regex literal may not start with space; add backslash to escape}} {{6-6=\}}

_ = /\)/
_ = /)/ // expected-error {{closing ')' does not balance any groups openings}}

_ = /,/
_ = /}/
_ = /]/
_ = /:/
_ = /;/

// Don't emit diagnostics here, as we re-lex.
_ = /0xG/
_ = /0oG/
_ = /"/
_ = /'/
_ = /<#placeholder#>/

_ = ^^/0xG/
_ = ^^/0oG/
_ = ^^/"/
_ = ^^/'/
_ = ^^/<#placeholder#>/

_ = (^^/0xG/)
_ = (^^/0oG/)
_ = (^^/"/)
_ = (^^/'/)
_ = (^^/<#placeholder#>/)
