// RUN: %target-parse-verify-swift

// Test function requirements within protocols, as well as conformance to
// said protocols.

// Simple function
protocol P1 {
  func f0()
}

// Simple match
struct X1a : P1 {
  func f0() {}
}

// Simple match selecting among two overloads.
struct X1b : P1 {
  func f0() -> Int {}
  func f0() {}
}

// Function with an associated type
protocol P2 {
  typealias Assoc : P1
  func f1(x: Assoc) // expected-note {{protocol requires function 'f1' with type '(X2x.Assoc) -> ()'}} expected-note{{protocol requires function 'f1' with type 'Assoc -> ()'}} expected-note{{protocol requires function 'f1' with type 'Assoc -> ()'}} expected-note{{multiple matching functions named 'f1' with type '(X2z.Assoc) -> ()'}}
}

// Exact match.
struct X2a : P2 {
  typealias Assoc = X1a

  func f1(x: X1a) {}
}

// Select among overloads.
struct X2d : P2 {
  typealias Assoc = X1a

  func f1(x: Int) { }
  func f1(x: X1a) { }
}

struct X2e : P2 {
  typealias Assoc = X1a

  func f1(x: X1b) { }
  func f1(x: X1a) { }
}

// Select among overloads distinguished by name.
struct X2f : P2 {
  typealias Assoc = X1a
  func f1(#y: X1a) { }
  func f1(x: X1a) { }
}

// Infer associated type from function parameter
struct X2g : P2 {
  func f1(x: X1a) { }
}

// Static/non-static mismatch.
struct X2w : P2 { // expected-error{{type 'X2w' does not conform to protocol 'P2'}}
  typealias Assoc = X1a
  static func f1(x: X1a) { } // expected-note{{candidate operates on a type, not an instance as required}}
}

// Deduction of type that doesn't meet requirements
struct X2x : P2 { // expected-error{{type 'X2x' does not conform to protocol 'P2'}}
  func f1(#x: Int) { } // expected-note{{candidate has non-matching type '(x: Int) -> ()'}}
}

// Mismatch in parameter types
struct X2y : P2 { // expected-error{{type 'X2y' does not conform to protocol 'P2'}}
  typealias Assoc = X1a
  func f1(#x: X1b) { } // expected-note{{candidate has non-matching type '(x: X1b) -> ()'}}
}

// Ambiguous deduction
struct X2z : P2 { // expected-error{{type 'X2z' does not conform to protocol 'P2'}}
  func f1(x: X1a) { } // expected-note{{candidate exactly matches [with Assoc = (X1a)]}}
  func f1(x: X1b) { } // expected-note{{candidate exactly matches [with Assoc = (X1b)]}}
}

// Protocol with prefix unary function
prefix operator ~~ {}

protocol P3 {
  typealias Assoc : P1
  prefix func ~~(_: Self) -> Assoc // expected-note{{protocol requires function '~~' with type 'X3z -> Assoc'}}
}

// Global operator match
struct X3a : P3 {
  typealias Assoc = X1a
}

prefix func ~~(_: X3a) -> X1a {} // expected-note{{candidate has non-matching type '(X3a) -> X1a'}} expected-note{{candidate is prefix, not postfix as required}}

// FIXME: Add example with overloaded prefix/postfix

// Prefix/postfix mismatch.
struct X3z : P3 { // expected-error{{type 'X3z' does not conform to protocol 'P3'}}
  typealias Assoc = X1a
}

postfix func ~~(_: X3z) -> X1a {} // expected-note{{candidate is postfix, not prefix as required}} expected-note{{candidate has non-matching type '(X3z) -> X1a'}}

// Protocol with postfix unary function
postfix operator ~~ {}
protocol P4 {
  typealias Assoc : P1
  postfix func ~~ (_: Self) -> Assoc // expected-note{{protocol requires function '~~' with type 'X4z -> Assoc'}}
}

// Global operator match
struct X4a : P4 {
  typealias Assoc = X1a
}

postfix func ~~(_: X4a) -> X1a {} // expected-note{{candidate has non-matching type '(X4a) -> X1a'}} expected-note{{candidate is postfix, not prefix as required}}

// Prefix/postfix mismatch.
struct X4z : P4 { // expected-error{{type 'X4z' does not conform to protocol 'P4'}}
  typealias Assoc = X1a
}

prefix func ~~(_: X4z) -> X1a {} // expected-note{{candidate has non-matching type '(X4z) -> X1a'}} expected-note{{candidate is prefix, not postfix as required}}

// Objective-C protocol
@objc protocol P5 {
  func f2(x: Int, withInt a: Int) // expected-note{{requirement 'f2(_:withInt:)' declared here}}
  func f2(x: Int, withOtherInt a: Int) // expected-note{{protocol requires function 'f2(_:withOtherInt:)' with type '(Int, withOtherInt: Int) -> ()'}}
}

// Exact match.
class X5a : P5 {
  @objc func f2(x: Int, withInt a: Int) {}
  @objc func f2(x: Int, withOtherInt a: Int) {}
}

// Body parameter names can vary.
class X5b : P5 {
  @objc func f2(y: Int, withInt a: Int) {}
  @objc func f2(y: Int, withOtherInt a: Int) {}
}

class X5c : P5 {
  @objc func f2(y: Int, withInt b: Int) {}
  @objc func f2(y: Int, withOtherInt b: Int) {}
}

// Cannot change name of the selector.
class X5d : P5 { // expected-error{{type 'X5d' does not conform to protocol 'P5'}}
  @objc func f2(#y: Int, withInt a: Int) {} // expected-note{{candidate has non-matching type '(y: Int, withInt: Int) -> ()'}} 
  // expected-error @-1{{method 'f2(y:withInt:)' has different argument names from those required by protocol 'P5' ('f2(_:withInt:)')}}
  @objc func f2(#y: Int, withOtherValue a: Int) {} // expected-note{{candidate has non-matching type '(y: Int, withOtherValue: Int) -> ()'}}
}

// Distinguish names within tuple arguments.
typealias T0 = (x: Int, y: String)
typealias T1 = (xx: Int, y: String)

func f(args: T0) {
}

func f(args: T1) {
}

f(T0(1, "Hi"))

infix operator ~>> { precedence 255 }

func ~>> (x: Int, args: T0) { println("T0") }
func ~>> (x: Int, args: T1) { println("T1") }

3~>>T0(1, "Hi")
3~>>T1(2, "Hi")

protocol Crankable {
  func ~>> (x: Self, args: T0)
  func ~>> (x: Self, args: T1)
}

extension Int : Crankable {}

// Invalid witnesses.
protocol P6 {
  func foo(x: Int)
  func bar(#x: Int) // expected-note{{protocol requires function 'bar(x:)' with type '(x: Int) -> ()'}}
}
struct X6 : P6 { // expected-error{{type 'X6' does not conform to protocol 'P6'}}
  func foo(x: Missing) { } // expected-error{{use of undeclared type 'Missing'}}
  func bar() { } // expected-note{{candidate has non-matching type '() -> ()'}}
}

protocol P7 {
  func foo(x: Blarg) // expected-error{{use of undeclared type 'Blarg'}}
}

struct X7 : P7 { }

// Selecting the most specialized witness.
prefix operator %%% {}

protocol P8 {
  func foo()
}

prefix func %%% <T : P8>(x: T) -> T { }

protocol P9 : P8 {
  prefix func %%% (x: Self) -> Self
}

struct X9 : P9 {
  func foo() {}
}

prefix func %%%(x: X9) -> X9 { }

protocol P10 {
  typealias Assoc
  func bar(x: Assoc)
}

struct X10 : P10 {
  typealias Assoc = Int
  func bar(x: Int) { }
  func bar<T>(x: T) { }
}

// <rdar://problem/15715996>
// FIXME: This should type-check; for now, we're testing that it does
// not crash.
protocol P11 {
  func ==(x: Self, y: Self) -> Bool
}

protocol P12 {
  typealias Index : P1
  func getIndex() -> Index // expected-note{{protocol requires function 'getIndex()' with type '() -> X12.Index'}}
}

struct XIndexType : P11 { } // expected-error{{type 'XIndexType' does not conform to protocol 'P11'}}

struct X12 : P12 { // expected-error{{type 'X12' does not conform to protocol 'P12'}}
  func getIndex() -> XIndexType { return XIndexType() } // expected-note{{candidate has non-matching type '() -> XIndexType'}}
}

func ==(x: X12.Index, y: X12.Index) -> Bool { return true } // expected-error 2{{'Index' is not a member type of 'X12'}}
