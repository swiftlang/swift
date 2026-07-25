// RUN: %target-typecheck-verify-swift %s -I %S/Inputs

enum E1 {
  case e1
  case e2
  case e3
}

func foo1(_ e: E1) {
  switch e {
//expected-error@-1 {{switch must be exhaustive}}
//expected-note@-2 {{add missing cases: '.e1', '.e2', '.e3'}} {{+3:3-3=case .e1:\n<#code#>\ncase .e2:\n<#code#>\ncase .e3:\n<#code#>\n}}
  }
}

func foo1 (_ i : Int) {
  switch i { // expected-error {{'switch' statement body must have at least one 'case' or 'default' block; add a default case}} {{+1:3-3=default:\n<#code#>\n}}
  }
}
