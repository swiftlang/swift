// RUN: %target-typecheck-verify-swift %s %S/Inputs/fixits-enum-multifile.swift -I %S/Inputs

func foo1(_ e: EMulti) {
  switch e {
// expected-error@-1 {{switch must be exhaustive}}
// expected-note@-2 {{add missing cases: '.e1', '.e2', '.e3(_)'}} {{+3:3-3=case .e1:\n<#code#>\ncase .e2:\n<#code#>\ncase .e3(_):\n<#code#>\n}}
  }
}
