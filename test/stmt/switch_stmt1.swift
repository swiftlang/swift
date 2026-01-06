// RUN: %target-typecheck-verify-swift

enum E {
  case e1
  case e2
}

func foo1(e : E, i : Int) {
  switch e {} // expected-error{{switch must be exhaustive}}
  // expected-note@-1 {{add missing cases}}{{13-13=case .e1:\n<#code#>\ncase .e2:\n<#code#>\n}}
  // expected-note@-2 {{missing case: '.e1'}}
  // expected-note@-3 {{missing case: '.e2'}}
  switch i {} // expected-error{{'switch' statement body must have at least one 'case' or 'default' block; add a default case}}{{13-13=default:\n<#code#>\n}}
}
