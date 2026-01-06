// RUN: %target-typecheck-verify-swift

protocol P {
  associatedtype A
}

protocol Q: P {
  associatedtype A
  func f() -> A
}

protocol R: Q {}

// We don't have enough information to infer 'A' just from the 'S1: P' and
// 'S2: P' conformances. Make sure that if we force one of those conformances
// first, we jump up to the conformance to Q, which has a requirement 'f()'
// that gives us a way to infer 'A'.

func forceP<T: P>(_: T) -> T.A {}

let x: Int = forceP(S1())

struct S1: Q {
  func f() -> Int {}
}

let y: String = forceP(S2())

struct S2: R {
  func f() -> String {}
}
