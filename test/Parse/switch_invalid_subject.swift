// RUN: %target-typecheck-verify-swift

func parseError1(x: Int) {
  switch func {} // expected-cparser-error {{expected expression in 'switch' statement}} expected-astgen-error {{expected identifier and function signature in function}} expected-cparser-error {{expected identifier in function declaration}} expected-astgen-error {{expected expression and '{}' to end 'switch' statement}} expected-astgen-error {{'switch' statement body must have at least one 'case' or 'default' block; add a default case}}
}
