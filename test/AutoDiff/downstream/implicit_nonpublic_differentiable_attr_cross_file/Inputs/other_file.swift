protocol Protocol: Differentiable {
  // expected-note @+2 {{protocol requires function 'internalMethod1' with type '(Float) -> Float'}}
  @differentiable(wrt: (self, x))
  func internalMethod1(_ x: Float) -> Float

  // expected-note @+3 {{protocol requires function 'internalMethod2' with type '(Float) -> Float'}}
  @differentiable(wrt: (self, x))
  @differentiable(wrt: x)
  func internalMethod2(_ x: Float) -> Float

  @differentiable(wrt: (self, x))
  @differentiable(wrt: x)
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
  // Expected: errors for missing `@differentiable` attribute.
  // expected-note @+1 {{candidate is missing attribute '@differentiable'}}
  func internalMethod1(_ x: Float) -> Float {
    x
  }

  // Expected: errors for missing `@differentiable` superset attribute.
  // expected-note @+2 {{candidate is missing attribute '@differentiable'}}
  @differentiable(wrt: x)
  func internalMethod2(_ x: Float) -> Float {
    x
  }

  // Expected: no errors for missing `@differentiable` subset attribute.
  @differentiable(wrt: (self, x))
  func internalMethod3(_ x: Float) -> Float {
    x
  }
}
