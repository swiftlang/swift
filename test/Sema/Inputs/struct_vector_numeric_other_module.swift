// SWIFT_ENABLE_TENSORFLOW

// expected-note @+1 {{type declared here}}
struct OtherFileNonconforming : AdditiveArithmetic {
  var float: Float
}

// expected-note @+1 {{type declared here}}
struct GenericOtherFileNonconforming<T : VectorNumeric> : AdditiveArithmetic {
  var x: T
  var y: T
}
