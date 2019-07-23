// RUN: %target-typecheck-verify-swift -enable-objc-interop

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
  associatedtype Assoc : P1 // expected-note{{ambiguous inference of associated type 'Assoc': 'X1a' vs. 'X1b'}}
  // expected-note@-1{{protocol requires nested type 'Assoc'}}
  func f1(_ x: Assoc) // expected-note{{protocol requires function 'f1' with type '(X2w.Assoc) -> ()'}} expected-note{{protocol requires function 'f1' with type '(X2y.Assoc) -> ()'}}
}

// Exact match.
struct X2a : P2 {
  typealias Assoc = X1a

  func f1(_ x: X1a) {}
}

// Select among overloads.
struct X2d : P2 {
  typealias Assoc = X1a

  func f1(_ x: Int) { }
  func f1(_ x: X1a) { }
}

struct X2e : P2 {
  typealias Assoc = X1a

  func f1(_ x: X1b) { }
  func f1(_ x: X1a) { }
}

// Select among overloads distinguished by name.
struct X2f : P2 {
  typealias Assoc = X1a
  func f1(y: X1a) { }
  func f1(_ x: X1a) { }
}

// Infer associated type from function parameter
struct X2g : P2 {
  func f1(_ x: X1a) { }
}

// Static/non-static mismatch.
struct X2w : P2 { // expected-error{{type 'X2w' does not conform to protocol 'P2'}}
  typealias Assoc = X1a
  static func f1(_ x: X1a) { } // expected-note{{candidate operates on a type, not an instance as required}}
}

// Deduction of type that doesn't meet requirements
struct X2x : P2 { // expected-error{{type 'X2x' does not conform to protocol 'P2'}}
  func f1(x: Int) { }
}

// Mismatch in parameter types
struct X2y : P2 { // expected-error{{type 'X2y' does not conform to protocol 'P2'}}
  typealias Assoc = X1a
  func f1(x: X1b) { }
}

// Ambiguous deduction
struct X2z : P2 { // expected-error{{type 'X2z' does not conform to protocol 'P2'}}
  func f1(_ x: X1a) { } // expected-note{{matching requirement 'f1' to this declaration inferred associated type to 'X1a'}}
  func f1(_ x: X1b) { } // expected-note{{matching requirement 'f1' to this declaration inferred associated type to 'X1b'}}
}

// Protocol with prefix unary function
prefix operator ~~

protocol P3 {
  associatedtype Assoc : P1
  static prefix func ~~(_: Self) -> Assoc // expected-note{{protocol requires function '~~' with type '(X3z) -> X3z.Assoc'}}
}

// Global operator match
struct X3a : P3 {
  typealias Assoc = X1a
}

prefix func ~~(_: X3a) -> X1a {}

// FIXME: Add example with overloaded prefix/postfix

// Prefix/postfix mismatch.
struct X3z : P3 { // expected-error{{type 'X3z' does not conform to protocol 'P3'}}
  typealias Assoc = X1a
}

postfix func ~~(_: X3z) -> X1a {} // expected-note{{candidate is postfix, not prefix as required}}

// Protocol with postfix unary function
postfix operator ~~
protocol P4 {
  associatedtype Assoc : P1
  static postfix func ~~ (_: Self) -> Assoc // expected-note{{protocol requires function '~~' with type '(X4z) -> X4z.Assoc'}}
}

// Global operator match
struct X4a : P4 {
  typealias Assoc = X1a
}

postfix func ~~(_: X4a) -> X1a {}

// Prefix/postfix mismatch.
struct X4z : P4 { // expected-error{{type 'X4z' does not conform to protocol 'P4'}}
  typealias Assoc = X1a
}

prefix func ~~(_: X4z) -> X1a {} // expected-note{{candidate is prefix, not postfix as required}}

// Objective-C protocol
@objc protocol P5 {
  func f2(_ x: Int, withInt a: Int)
  func f2(_ x: Int, withOtherInt a: Int)
}

// Exact match.
class X5a : P5 {
  @objc func f2(_ x: Int, withInt a: Int) {}
  @objc func f2(_ x: Int, withOtherInt a: Int) {}
}

// Body parameter names can vary.
class X5b : P5 {
  @objc func f2(_ y: Int, withInt a: Int) {}
  @objc func f2(_ y: Int, withOtherInt a: Int) {}
}

class X5c : P5 {
  @objc func f2(_ y: Int, withInt b: Int) {}
  @objc func f2(_ y: Int, withOtherInt b: Int) {}
}

// Names need to match up for an Objective-C protocol as well.
class X5d : P5 {
  @objc(f2WithY:withInt:) func f2(_ y: Int, withInt a: Int) {} // expected-error {{Objective-C method 'f2WithY:withInt:' provided by method 'f2(_:withInt:)' does not match the requirement's selector ('f2:withInt:')}}
  @objc(f2WithY:withOtherValue:) func f2(_ y: Int, withOtherInt a: Int) {} // expected-error{{Objective-C method 'f2WithY:withOtherValue:' provided by method 'f2(_:withOtherInt:)' does not match the requirement's selector ('f2:withOtherInt:')}}
}

// Distinguish names within tuple arguments.
typealias T0 = (x: Int, y: String)
typealias T1 = (xx: Int, y: String)

func f(_ args: T0) {
}

func f(_ args: T1) {
}

f(T0(1, "Hi"))

infix operator ~>> : MaxPrecedence
precedencegroup MaxPrecedence { higherThan: BitwiseShiftPrecedence }

func ~>> (x: Int, args: T0) {}
func ~>> (x: Int, args: T1) {}

3~>>T0(1, "Hi")
3~>>T1(2, "Hi")

protocol Crankable {
  static func ~>> (x: Self, args: T0)
  static func ~>> (x: Self, args: T1)
}

extension Int : Crankable {}

// Invalid witnesses.
protocol P6 {
  func foo(_ x: Int)
  func bar(x: Int) // expected-note{{protocol requires function 'bar(x:)' with type '(Int) -> ()'}}
}
struct X6 : P6 { // expected-error{{type 'X6' does not conform to protocol 'P6'}}
  func foo(_ x: Missing) { } // expected-error{{use of undeclared type 'Missing'}}
  func bar() { }
}

protocol P6Ownership {
  func thunk__shared(_ x: __shared Int)
  func mismatch__shared(_ x: Int)
  func mismatch__owned(x: Int)
  func thunk__owned__owned(x: __owned Int)

  __consuming func inherits__consuming(x: Int)
  func mismatch__consuming(x: Int)
  __consuming func mismatch__consuming_mutating(x: Int) // expected-note {{protocol requires function 'mismatch__consuming_mutating(x:)' with type '(Int) -> ()'}}
  mutating func mismatch__mutating_consuming(x: Int)
}
struct X6Ownership : P6Ownership { // expected-error{{type 'X6Ownership' does not conform to protocol 'P6Ownership'}}
  func thunk__shared(_ x: Int) { } // OK
  func mismatch__shared(_ x: __shared Int) { } // OK
  func mismatch__owned(x: __owned Int) { } // OK
  func thunk__owned__owned(x: Int) { } // OK

  func inherits__consuming(x: Int) { } // OK
  __consuming func mismatch__consuming(x: Int) { } // OK
  mutating func mismatch__consuming_mutating(x: Int) { } // expected-note {{candidate is marked 'mutating' but protocol does not allow it}}
  __consuming func mismatch__mutating_consuming(x: Int) { } // OK - '__consuming' acts as a counterpart to 'nonmutating'
}

protocol P7 {
  func foo(_ x: Blarg) // expected-error{{use of undeclared type 'Blarg'}}
}

struct X7 : P7 { }

// Selecting the most specialized witness.
prefix operator %%%

protocol P8 {
  func foo()
}

prefix func %%% <T : P8>(x: T) -> T { }

protocol P9 : P8 {
  static prefix func %%% (x: Self) -> Self
}

struct X9 : P9 {
  func foo() {}
}

prefix func %%%(x: X9) -> X9 { }

protocol P10 {
  associatedtype Assoc
  func bar(_ x: Assoc)
}

struct X10 : P10 {
  typealias Assoc = Int
  func bar(_ x: Int) { }
  func bar<T>(_ x: T) { }
}

protocol P11 {
  static func ==(x: Self, y: Self) -> Bool
}

protocol P12 {
  associatedtype Index : P1 // expected-note{{unable to infer associated type 'Index' for protocol 'P12'}}
  func getIndex() -> Index
}

struct XIndexType : P11 { }

struct X12 : P12 { // expected-error{{type 'X12' does not conform to protocol 'P12'}}
  func getIndex() -> XIndexType { return XIndexType() } // expected-note{{candidate would match and infer 'Index' = 'XIndexType' if 'XIndexType' conformed to 'P1'}}
}

func ==(x: X12.Index, y: X12.Index) -> Bool { return true }

protocol P13 {}
protocol P14 {
  static prefix func %%%(_: Self.Type)
}
prefix func %%%<P: P13>(_: P.Type) { }
struct X13: P14, P13 { }
