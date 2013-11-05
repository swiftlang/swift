// RUN: %swift -parse %s -verify

// Test function requirements within protocols, as well as conformance to
// said protocols.

// Simple function
protocol P1 {
  def f0()
}

// Simple match
struct X1a : P1 {
  def f0() {} 
}

// Simple match selecting among two overloads.
struct X1b : P1 {
  def f0() -> Int {}
  def f0() {}
}

// Function with an associated type
protocol P2 {
  typealias Assoc : P1
  def f1(x: Assoc) // expected-note{{protocol requires function 'f1' with type '(x: X2x.Assoc) -> ()'}} expected-note{{protocol requires function 'f1' with type '(x: Assoc) -> ()'}} expected-note{{protocol requires function 'f1' with type '(x: Assoc) -> ()'}} expected-note{{multiple matching functions named 'f1' with type '(x: X2z.Assoc) -> ()'}}
}

// Exact match.
struct X2a : P2 {
  typealias Assoc = X1a

  def f1(x: X1a) {}
}

// Matches with different function parameter names.
struct X2b : P2 {
  typealias Assoc = X1a

  def f1(y: X1a) {}
}

struct X2c : P2 {
  typealias Assoc = X1a

  def f1(_: X1a) {}
}

// Select among overloads.
struct X2d : P2 {
  typealias Assoc = X1a

  def f1(x: Int) { }
  def f1(x: X1a) { }
}

struct X2e : P2 {
  typealias Assoc = X1a

  def f1(x: X1b) { }
  def f1(x: X1a) { }
}

// Select among overloads distinguished by name.
struct X2f : P2 {
  typealias Assoc = X1a
  def f1(y: X1a) { }
  def f1(x: X1a) { }
}

// Infer associated type from function parameter
struct X2g : P2 {
  def f1(x: X1a) { }
}

// Static/non-static mismatch.
struct X2w : P2 { // expected-error{{type 'X2w' does not conform to protocol 'P2'}}
  typealias Assoc = X1a
  static def f1(x: X1a) { } // expected-note{{candidate is 'static', but requirement is not}}
}

// Deduction of type that doesn't meet requirements
struct X2x : P2 { // expected-error{{type 'X2x' does not conform to protocol 'P2'}}
  def f1(x: Int) { } // expected-note{{candidate has non-matching type '(x: Int) -> ()'}}
}

// Mismatch in parameter types
struct X2y : P2 { // expected-error{{type 'X2y' does not conform to protocol 'P2'}}
  typealias Assoc = X1a
  def f1(x: X1b) { } // expected-note{{candidate has non-matching type '(x: X1b) -> ()'}}
}

// Ambiguous deduction
struct X2z : P2 { // expected-error{{type 'X2z' does not conform to protocol 'P2'}}
  def f1(x: X1a) { } // expected-note{{candidate exactly matches [with Assoc = X1a]}}
  def f1(x: X1b) { } // expected-note{{candidate exactly matches [with Assoc = X1b]}}
}

// Protocol with prefix unary function
operator prefix ~~ {}

protocol P3 {
  typealias Assoc : P1
  @prefix def ~~(_: Self) -> Assoc // expected-note{{protocol requires function '~~' with type 'X3z -> Assoc'}}
}

// Global operator match
struct X3a : P3 {
  typealias Assoc = X1a
}

@prefix def ~~(_: X3a) -> X1a {} // expected-note{{candidate has non-matching type 'X3a -> X1a'}} expected-note{{candidate is prefix, not postfix as required}}

// FIXME: Add example with overloaded prefix/postfix

// Prefix/postfix mismatch.
struct X3z : P3 { // expected-error{{type 'X3z' does not conform to protocol 'P3'}}
  typealias Assoc = X1a
}

@postfix def ~~(_: X3z) -> X1a {} // expected-note{{candidate is postfix, not prefix as required}} expected-note{{candidate has non-matching type 'X3z -> X1a'}}

// Protocol with postfix unary function
operator postfix ~~ {}
protocol P4 {
  typealias Assoc : P1
  @postfix def ~~ (_: Self) -> Assoc // expected-note{{protocol requires function '~~' with type 'X4z -> Assoc'}}
}

// Global operator match
struct X4a : P4 {
  typealias Assoc = X1a
}

@postfix def ~~(_: X4a) -> X1a {} // expected-note{{candidate has non-matching type 'X4a -> X1a'}} expected-note{{candidate is postfix, not prefix as required}}

// Prefix/postfix mismatch.
struct X4z : P4 { // expected-error{{type 'X4z' does not conform to protocol 'P4'}}
  typealias Assoc = X1a
}

@prefix def ~~(_: X4z) -> X1a {} // expected-note{{candidate has non-matching type 'X4z -> X1a'}} expected-note{{candidate is prefix, not postfix as required}}

// Objective-C protocol
@class_protocol @objc protocol P5 {
  def f2(x: Int, withInt: Int)
  def f2(x: Int, withOtherInt: Int) // expected-note{{protocol requires function 'f2' with type '(x: Int, withOtherInt: Int) -> ()'}}
}

// Exact match.
class X5a : P5 {
  @objc def f2(x: Int, withInt: Int) {}
  @objc def f2(x: Int, withOtherInt: Int) {}
}

// Name of the first parameter can vary.
class X5b : P5 {
  @objc def f2(y: Int, withInt: Int) {}
  @objc def f2(y: Int, withOtherInt: Int) {}
}

// Cannot change name of second parameter.
class X5c : P5 { // expected-error{{type 'X5c' does not conform to protocol 'P5'}}
  @objc def f2(y: Int, withInt: Int) {} // expected-note{{candidate has non-matching type '(y: Int, withInt: Int) -> ()'}}
  @objc def f2(y: Int, withOtherValue: Int) {} // expected-note{{candidate has non-matching type '(y: Int, withOtherValue: Int) -> ()'}}
}

// Distinguish names within tuple arguments.
typealias T0 = (x: Int, y: String)
typealias T1 = (xx: Int, y: String)

def f(args: T0) {
}

def f(args: T1) {
}

f(T0(1, "Hi"))

operator infix ~>> { precedence 255 }

def ~>> (x: Int, args: T0) { println("T0") }
def ~>> (x: Int, args: T1) { println("T1") }

3~>>T0(1, "Hi")
3~>>T1(2, "Hi")

protocol Crankable {
  def ~>> (x: Self, args: T0)
  def ~>> (x: Self, args: T1)
}

extension Int : Crankable {}

// Invalid witnesses.
protocol P6 {
  def foo(x: Int)
  def bar(x: Int) // expected-note{{protocol requires function 'bar' with type '(x: Int) -> ()'}}
}
struct X6 : P6 { // expected-error{{type 'X6' does not conform to protocol 'P6'}}
  def foo(x: Missing) { } // expected-error{{use of undeclared type 'Missing'}}
  def bar() { } // expected-note{{candidate has non-matching type '() -> ()'}}
}

protocol P7 {
  def foo(x: Blarg) // expected-error{{use of undeclared type 'Blarg'}}
}

struct X7 : P7 { }

// Selecting the most specialized witness.
operator prefix %%% {}

protocol P8 {
  def foo()
}

@prefix def %%% <T : P8>(x: T) -> T { }

protocol P9 : P8 {
  @prefix def %%% (x: Self) -> Self
}

struct X9 : P9 {
  def foo() {}  
}

@prefix def %%%(x: X9) -> X9 { }

protocol P10 {
  typealias Assoc
  def bar(x: Assoc)
}

struct X10 : P10 {
  typealias Assoc = Int
  def bar(x: Int) { }
  def bar<T>(x: T) { }
}

