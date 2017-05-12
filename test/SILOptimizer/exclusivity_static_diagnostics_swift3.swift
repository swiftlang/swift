// RUN: %target-swift-frontend -enforce-exclusivity=checked -swift-version 3 -emit-sil -primary-file %s -o /dev/null -verify

import Swift

// In Swift 3 compatibility mode, diagnostics for exclusive accesses are
// warnings not errors.

func takesTwoInouts<T>(_ p1: inout T, _ p2: inout T) { }

func simpleInoutDiagnostic() {
  var i = 7

  // expected-error@+4{{inout arguments are not allowed to alias each other}}
  // expected-note@+3{{previous aliasing argument}}
  // expected-warning@+2{{simultaneous accesses to var 'i', but modification requires exclusive access; consider copying to a local variable}}
  // expected-note@+1{{conflicting access is here}}
  takesTwoInouts(&i, &i)
}

struct X {
  var f = 12
}

func diagnoseOnSameField() {
  var x = X()

  // expected-warning@+2{{simultaneous accesses to var 'x', but modification requires exclusive access; consider copying to a local variable}}
  // expected-note@+1{{conflicting access is here}}
  takesTwoInouts(&x.f, &x.f)
}
