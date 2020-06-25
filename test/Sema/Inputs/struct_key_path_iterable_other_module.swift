// SWIFT_ENABLE_TENSORFLOW

// expected-note @+1 {{type declared here}}
struct OtherFileNonconforming {
  var int: Int
  var float: Float
}

// expected-note @+1 {{type declared here}}
struct GenericOtherFileNonconforming<T : KeyPathIterable> {
  var x: T
  var int: Int
  var float: Float
}
