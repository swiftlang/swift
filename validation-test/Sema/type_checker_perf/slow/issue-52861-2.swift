// RUN: %target-typecheck-verify-swift -solver-scope-threshold=1000

// Further down in https://github.com/swiftlang/swift/issues/52861:

// This type checks in ~300ms with the default limits, but we make it fail sooner.

func slow() {
  let u = SIMD2<Float>(0, 1)
  let v = SIMD2<Float>(1, 2)
  let r = [ // expected-error{{reasonable time}}
      2*u + 3*v,
      4*u + 5*v
  ]
  print(r)
}
