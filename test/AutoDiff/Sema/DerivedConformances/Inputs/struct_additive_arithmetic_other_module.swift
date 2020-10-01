// expected-note @+1 3 {{type declared here}}
struct OtherFileNonconforming: Equatable {
  var int: Int
  var float: Float
}

// expected-note @+1 3 {{type declared here}}
struct GenericOtherFileNonconforming<T: AdditiveArithmetic>: Equatable {
  var x: T
  var int: Int
  var float: Float
}
