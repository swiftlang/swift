struct S {
  var a = S() // expected-error {{'S' cannot be constructed because it has no accessible initializers}}
}
