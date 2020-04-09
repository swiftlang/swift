// RUN: not %target-swift-frontend-typecheck -parse-stdlib %s

// SR-12559: `@derivative` attribute type-checking crash.
// This program is not valid, but the compiler should not crash nonetheless.
// Reproducible only with `-parse-stdlib`.

// The crash occurs because it is not sufficient for attribute type-checking to
// check for `Differentiable` conformances. We must also check for invalid
// associated types:
//
//   (lldb) p valueResultConf.dump()
//   (normal_conformance type=AnyDerivative protocol=Differentiable
//     (assoc_type req=TangentVector type=<<error type>>))

import _Differentiation

struct AnyDerivative: Differentiable {
  init<T>(_ base: T) {}

  @derivative(of: init)
  static func _vjpInit<T: Differentiable>(
    _ base: T
  ) -> (value: AnyDerivative, pullback: (AnyDerivative) -> T.TangentVector) {
    fatalError()
  }

  typealias TangentVector = AnyDerivative
}

// Assertion failed: (resultTan && "Original result has no tangent space?"), function getAutoDiffDerivativeFunctionLinearMapType, file /Users/danielzheng/swift-merge/swift/lib/AST/Type.cpp, line 5190.
