// RUN: %target-typecheck-verify-swift -solver-scope-threshold=10000
// This also fails with the default limit.

// https://github.com/swiftlang/swift/issues/60320

struct MyType {}
func *(_: Double, _: MyType) -> MyType {}
// func *(_: MyType, _: Double) -> MyType {}
func +(_: MyType, _: MyType) -> MyType {}

func slow() {
  let d: Double
  let state: MyType
  let k1: MyType
  let k2: MyType
  let k3: MyType
  let k4: MyType

  // This is invalid because of the missing overload commented out above
  let result = state + (1.0/6) * (k1 + 2*k2 + 2*k3 + k4) * d
  // expected-error@-1 {{reasonable}}
}
