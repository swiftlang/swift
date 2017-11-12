// RUN: %target-typecheck-verify-swift -diagnostics-editor-mode

enum E {
  case e1
  case e2
}

func foo1(e : E, i : Int) {
  switch e {} // expected-error{{switch must be exhaustive}}
  // expected-note@-1{{do you want to add missing cases?}}{{13-13=case .e1:\n<#code#>\ncase .e2:\n<#code#>\n}}
  switch i {} // expected-error{{'switch' statement body must have at least one 'case' or 'default' block; do you want to add a default case?}}{{13-13=default:\n<#code#>\n}}
}
