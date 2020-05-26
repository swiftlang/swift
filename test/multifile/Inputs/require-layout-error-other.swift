public struct S {
  var x: DoesNotExist // expected-error {{cannot find type 'DoesNotExist' in scope}}
}
