// SWIFT_ENABLE_TENSORFLOW

// expected-note @+1 3 {{type declared here}}
struct OtherFileNonconforming : Equatable {
}

// expected-note @+1 3 {{type declared here}}
struct GenericOtherFileNonconforming<T : PointwiseMultiplicative> : Equatable {
  var x: T
  var float: Float
  var double: Double
}
