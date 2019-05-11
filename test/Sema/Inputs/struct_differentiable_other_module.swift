// SWIFT_ENABLE_TENSORFLOW

// expected-note @+1 {{type declared here}}
struct OtherFileNonconforming {}

// expected-note @+1 {{type declared here}}
struct GenericOtherFileNonconforming<T : Differentiable> {
  var x: T
}
