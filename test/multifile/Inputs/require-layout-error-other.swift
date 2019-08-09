public struct S {
  var x: DoesNotExist // expected-error {{use of undeclared type 'DoesNotExist'}}
}
