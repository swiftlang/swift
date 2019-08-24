// RUN: %target-typecheck-verify-swift

enum E {
  case e1
  case e2
}

func foo1(e : E, i : Int) {
  switch e {} // expected-error{{switch must be exhaustive}}
  // expected-note@-1 {{missing case: '.e1'}}
  // expected-note@-2 {{missing case: '.e2'}}
  switch i {} // expected-error{{'switch' statement body must have at least one 'case' or 'default' block; do you want to add a default case?}}{{13-13=default:\n<#code#>\n}}
}
