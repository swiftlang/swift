// RUN: %target-typecheck-verify-swift

enum E {
  case e1
  case e2
}

func foo1(e : E, i : Int) {
  switch e {} // expected-error{{'switch' statement body must have at least one 'case' or 'default' block; do you want to add missing cases?}}{{13-13=case .e1: <#code#>\ncase .e2: <#code#>\n}}
  switch i {} // expected-error{{'switch' statement body must have at least one 'case' or 'default' block; do you want to add a default case?}}{{13-13=default: <#code#>\n}}
}
