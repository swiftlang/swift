// RUN: %target-typecheck-verify-swift

// Test constraint simplification of chains of binary operators.
// <https://bugs.swift.org/browse/SR-1122>
do {
  let a: String? = "a"
  let b: String? = "b"
  let c: String? = "c"
  let _: String? = a! + b! + c!

  let x: Double = 1
  _ = x + x + x

  let sr3483: Double? = 1
  _ = sr3483! + sr3483! + sr3483!

  let sr2636: [String: Double] = ["pizza": 10.99, "ice cream": 4.99, "salad": 7.99]
  _ = sr2636["pizza"]!
  _ = sr2636["pizza"]! + sr2636["salad"]!
  _ = sr2636["pizza"]! + sr2636["salad"]! + sr2636["ice cream"]!
}

// Use operators defined within a type.
struct S0 {
  static func +(lhs: S0, rhs: S0) -> S0 { return lhs }
}

func useS0(lhs: S0, rhs: S0) { 
  _ = lhs + rhs
}

// Use operators defined within a generic type.
struct S0b<T> {
  static func + <U>(lhs: S0b<T>, rhs: U) -> S0b<U> { return S0b<U>() }
}

func useS0b(s1i: S0b<Int>, s: String) {
  var s1s = s1i + s
  s1s = S0b<String>()
  _ = s1s
}

// Use operators defined within a protocol extension.
infix operator %%%
infix operator %%%%

protocol P1 {
  static func %%%(lhs: Self, rhs: Self) -> Bool
}

extension P1 {
  static func %%%%(lhs: Self, rhs: Self) -> Bool {
    return !(lhs %%% rhs)
  }
}

func useP1Directly<T : P1>(lhs: T, rhs: T) {
  _ = lhs %%% rhs
  _ = lhs %%%% rhs
}

struct S1 : P1 {
  static func %%%(lhs: S1, rhs: S1) -> Bool { return false }
}

func useP1Model(lhs: S1, rhs: S1) {
  _ = lhs %%% rhs
  _ = lhs %%%% rhs
}

struct S1b<T> : P1 {
  static func %%%(lhs: S1b<T>, rhs: S1b<T>) -> Bool { return false }
}

func useP1ModelB(lhs: S1b<Int>, rhs: S1b<Int>) {
  _ = lhs %%% rhs
  _ = lhs %%%% rhs
}

func useP1ModelBGeneric<T>(lhs: S1b<T>, rhs: S1b<T>) {
  _ = lhs %%% rhs
  _ = lhs %%%% rhs
}

// Use operators defined within a protocol extension to satisfy a requirement.
protocol P2 {
  static func %%%(lhs: Self, rhs: Self) -> Bool
  static func %%%%(lhs: Self, rhs: Self) -> Bool
}

extension P2 {
  static func %%%%(lhs: Self, rhs: Self) -> Bool {
    return !(lhs %%% rhs)
  }
}

func useP2Directly<T : P2>(lhs: T, rhs: T) {
  _ = lhs %%% rhs
  _ = lhs %%%% rhs
}

struct S2 : P2 {
  static func %%%(lhs: S2, rhs: S2) -> Bool { return false }
}

func useP2Model(lhs: S2, rhs: S2) {
  _ = lhs %%% rhs
  _ = lhs %%%% rhs
}

struct S2b<T> : P2 {
  static func %%%(lhs: S2b<T>, rhs: S2b<T>) -> Bool { return false }
}

func useP2Model2(lhs: S2b<Int>, rhs: S2b<Int>) {
  _ = lhs %%% rhs
  _ = lhs %%%% rhs
}

func useP2Model2Generic<T>(lhs: S2b<T>, rhs: S2b<T>) {
  _ = lhs %%% rhs
  _ = lhs %%%% rhs
}

// Using an extension of one protocol to satisfy another conformance.
protocol P3 { }

extension P3 {
  static func ==(lhs: Self, rhs: Self) -> Bool {
    return true
  }  
}

struct S3 : P3, Equatable { }

// rdar://problem/30220565
func shrinkTooFar(_ : Double, closure : ()->()) {}
func testShrinkTooFar() {
  shrinkTooFar(0*0*0) {}
}
