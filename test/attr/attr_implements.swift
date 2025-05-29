// RUN: %target-run-simple-swift %s
// REQUIRES: executable_test

protocol P {
  func f0() -> Int;
  func f1() -> Int
  func f(x:Int, y:Int) -> Int;
}

protocol Q {
  func f(x:Int, y:Int) -> Int;
}

struct S : P, Q, Equatable {

  // Test that it's possible to denote a zero-arg requirement
  // (This involved extended the parser for unqualified DeclNames)
  @_implements(P, f0())
  func g0() -> Int {
    return 10
  }

  // Test that we can handle a non-identifier type that canonicalizes
  // to a protocol.
  @_implements((P), f1())
  func g1() -> Int { 11 }


  // Test that it's possible to implement two different protocols with the
  // same-named requirements.
  @_implements(P, f(x:y:))
  func g(x:Int, y:Int) -> Int {
    return 5
  }

  @_implements(Q, f(x:y:))
  func h(x:Int, y:Int) -> Int {
    return 6
  }

  // Test that it's possible to denote an operator requirement
  // (This involved extended the parser for unqualified DeclNames)
  @_implements(Equatable, ==(_:_:))
  public static func isEqual(_ lhs: S, _ rhs: S) -> Bool {
    return false
  }
}

func call_P_f_generic<T:P>(p:T, x: Int, y: Int) -> Int {
  return p.f(x:x, y:y)
}

func call_P_f_existential(p:P, x: Int, y: Int) -> Int {
  return p.f(x:x, y:y)
}

func call_Q_f_generic<T:Q>(q:T, x: Int, y: Int) -> Int {
  return q.f(x:x, y:y)
}

func call_Q_f_existential(q:Q, x: Int, y: Int) -> Int {
  return q.f(x:x, y:y)
}

let s = S()
assert(call_P_f_generic(p:s, x:1, y:2) == 5)
assert(call_P_f_existential(p:s, x:1, y:2) == 5)
assert(call_Q_f_generic(q:s, x:1, y:2) == 6)
assert(call_Q_f_existential(q:s, x:1, y:2) == 6)
assert(!(s == s))

// Note: at the moment directly calling the member 'f' on the concrete type 'S'
// doesn't work, because it's considered ambiguous between the 'g' and 'h'
// members (each of which provide an 'f' via the 'P' and 'Q' protocol
// conformances), and adding a non-@_implements member 'f' to S makes it win
// over _both_ the @_implements members. Unclear if this is correct; I think so?

// print(s.f(x:1, y:2))


// Next test is for rdar://43804798
//
// When choosing between an @_implements-provided implementation of a specific
// protocol's witness (Equatable / Comparable in particular), we want to choose
// the @_implements-provided one when we're looking up from a context that only
// knows the protocol bound, and the non-@_implements-provided one when we're
// looking up from a context that knows the full type.

struct SpecificType : Equatable {
  @_implements(Equatable, ==(_:_:))
  static func bar(_: SpecificType, _: SpecificType) -> Bool { return true }
  static func ==(_: SpecificType, _: SpecificType) -> Bool { return false }
}

func trueWhenJustEquatable<T: Equatable>(_ x: T) -> Bool { return x == x }
func falseWhenSpecificType(_ x: SpecificType) -> Bool { return x == x }

assert(trueWhenJustEquatable(SpecificType()))
assert(!falseWhenSpecificType(SpecificType()))

// @_implements on associated types
protocol PWithAssoc {
  associatedtype A
}

struct XWithAssoc: PWithAssoc {
  @_implements(PWithAssoc, A)
  typealias __P_A = Int
}
