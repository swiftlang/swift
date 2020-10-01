import _Differentiation

// expected-note @+1 2 {{type declared here}}
class OtherFileNonconforming {}

// expected-note @+1 2 {{type declared here}}
class GenericOtherFileNonconforming<T: Differentiable> {
  var x: T
}
