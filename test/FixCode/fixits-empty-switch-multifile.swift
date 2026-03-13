// RUN: %target-typecheck-verify-swift %s %S/Inputs/fixits-enum-multifile.swift -I %S/Inputs

func foo1(_ e: EMulti) {
  switch e {
// expected-error@-1 {{switch must be exhaustive}}
// expected-note@-2 {{add missing case: '.e1'}} {{+6:3-3=case .e1:\n<#code#>\n}}
// expected-note@-3 {{add missing case: '.e2'}} {{+6:3-3=case .e2:\n<#code#>\n}}
// expected-note@-4 {{add missing case: '.e3(_)'}} {{+6:3-3=case .e3(_):\n<#code#>\n}}
// expected-note@-5 {{add missing cases}} {{+6:3-3=case .e1:\n<#code#>\ncase .e2:\n<#code#>\ncase .e3(_):\n<#code#>\n}}
  }
}
