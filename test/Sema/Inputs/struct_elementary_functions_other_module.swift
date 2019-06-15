// SWIFT_ENABLE_TENSORFLOW

// expected-note @+1 24 {{type declared here}}
struct OtherFileNonconforming : Equatable {
  let float: Float
  var double: Double
}

// expected-note @+1 24 {{type declared here}}
struct GenericOtherFileNonconforming<T : ElementaryFunctions> : Equatable {
  let x: T
  var float: Float
  var double: Double
}
