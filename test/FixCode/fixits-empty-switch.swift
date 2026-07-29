// RUN: %target-typecheck-verify-swift %s -I %S/Inputs

enum E1 {
  case e1
  case e2
  case e3
}

func foo1(_ e: E1) {
  switch e {
//expected-error@-1 {{switch must be exhaustive}}
//expected-note@-2 {{add missing case: '.e1'}} {{+6:3-3=case .e1:\n<#code#>\n}}
//expected-note@-3 {{add missing case: '.e2'}} {{+6:3-3=case .e2:\n<#code#>\n}}
//expected-note@-4 {{add missing case: '.e3'}} {{+6:3-3=case .e3:\n<#code#>\n}}
//expected-note@-5 {{add missing cases}} {{+6:3-3=case .e1:\n<#code#>\ncase .e2:\n<#code#>\ncase .e3:\n<#code#>\n}}
  }
}

func foo1 (_ i : Int) {
  switch i { // expected-error {{'switch' statement body must have at least one 'case' or 'default' block; add a default case}} {{+1:3-3=default:\n<#code#>\n}}
  }
}
