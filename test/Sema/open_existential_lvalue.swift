// RUN: %target-typecheck-verify-swift

// rdar://121214563
// https://github.com/swiftlang/swift/issues/60619
// REQUIRES: rdar121214563

protocol Q {}

func takesAny(x: any Q) {}
func takesRValue(x: any Q) {}
func takesInOut(x: inout any Q) {}

struct S {
  subscript<T: Q>(_ ct: T.Type) -> T {
    get { fatalError() }
    set { fatalError() }
  }
}

func f(s: inout S, t: any Q.Type) -> (any Q) {
  takesAny(x: s[t])
  takesRValue(x: s[t])
  takesInOut(x: &s[t]) // expected-error {{cannot pass immutable value as inout argument: 's' is immutable}}
}
