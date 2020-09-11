// SWIFT_ENABLE_TENSORFLOW

struct Base : PointwiseMultiplicative {}

// expected-note @+1 3 {{type declared here}}
struct OtherFileNonconforming : Equatable, AdditiveArithmetic {
  var base: Base
}

// expected-note @+1 3 {{type declared here}}
struct GenericOtherFileNonconforming<
  T : PointwiseMultiplicative
> : Equatable, AdditiveArithmetic {
  var x: T
  var base: Base
}
