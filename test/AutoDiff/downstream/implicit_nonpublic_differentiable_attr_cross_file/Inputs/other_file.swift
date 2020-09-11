protocol Protocol: Differentiable {
  // expected-note @+2 {{protocol requires function 'internalMethod1' with type '(Float) -> Float'}}
  @differentiable(wrt: (self, x))
  func internalMethod1(_ x: Float) -> Float

  // expected-note @+3 {{protocol requires function 'internalMethod2' with type '(Float) -> Float'}}
  @differentiable(wrt: x)
  @differentiable(wrt: (self, x))
  func internalMethod2(_ x: Float) -> Float

  @differentiable(wrt: x)
  @differentiable(wrt: (self, x))
  func internalMethod3(_ x: Float) -> Float
}

protocol Protocol2: Differentiable {
  @differentiable(wrt: (self, x))
  func internalMethod4(_ x: Float) -> Float
}

// Note:
// - No `ConformingStruct: Protocol` conformance exists in this file, so this
//   file should compile just file.
// - A `ConformingStruct: Protocol` conformance in a different file should be
//   diagnosed to prevent linker errors. Without a diagnostic, compilation of
//   the other file creates external references to symbols for implicit
//   `@differentiable` attributes, even though no such symbols exist.
//   Context: https://github.com/apple/swift/pull/29771#issuecomment-585059721

struct ConformingStruct: Differentiable {
  // Expected: error for missing `@differentiable` attribute.
  // expected-note @+1 {{non-public instance method 'internalMethod1' must have explicit '@differentiable' attribute to satisfy requirement instance method 'internalMethod1' (in protocol 'Protocol') because it is declared in a different file than the conformance of 'ConformingStruct' to 'Protocol'}} {{3-3=@differentiable }}
  func internalMethod1(_ x: Float) -> Float {
    x
  }

  // Expected: error for missing `@differentiable` superset attribute.
  // expected-note @+2 {{non-public instance method 'internalMethod2' must have explicit '@differentiable' attribute to satisfy requirement instance method 'internalMethod2' (in protocol 'Protocol') because it is declared in a different file than the conformance of 'ConformingStruct' to 'Protocol'}} {{3-3=@differentiable }}
  @differentiable(wrt: x)
  func internalMethod2(_ x: Float) -> Float {
    x
  }

  // Expected: no error for missing `@differentiable` subset attribute.
  @differentiable(wrt: (self, x))
  func internalMethod3(_ x: Float) -> Float {
    x
  }
}
