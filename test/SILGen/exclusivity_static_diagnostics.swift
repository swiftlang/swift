// RUN: %target-swift-frontend -enforce-exclusivity=checked -swift-version 4 -emit-sil -primary-file %s -o /dev/null -verify

import Swift

func takesTwoInouts<T>(_ p1: inout T, _ p2: inout T) { }

func simpleInoutDiagnostic() {
  var i = 7

  // FIXME: This diagnostic should be removed if static enforcement is
  // turned on by default.
  // expected-error@+2{{inout arguments are not allowed to alias each other}}
  // expected-note@+1{{previous aliasing argument}}
  takesTwoInouts(&i, &i)
}

func inoutOnInoutParameter(p: inout Int) {
  // expected-error@+2{{inout arguments are not allowed to alias each other}}
  // expected-note@+1{{previous aliasing argument}}
  takesTwoInouts(&p, &p)
}

class SomeClass { }

struct StructWithMutatingMethodThatTakesSelfInout {
  var f = SomeClass()
  mutating func mutate(_ other: inout StructWithMutatingMethodThatTakesSelfInout) { }
  mutating func mutate(_ other: inout SomeClass) { }

  mutating func callMutatingMethodThatTakesSelfInout() {
    // expected-error@+2{{inout arguments are not allowed to alias each other}}
    // expected-note@+1{{previous aliasing argument}}
    mutate(&self)
  }

  mutating func callMutatingMethodThatTakesSelfStoredPropInout() {
    mutate(&self.f)
  }
}

func violationWithGenericType<T>(_ p: T) {
  var local = p
  // expected-error@+2{{inout arguments are not allowed to alias each other}}
  // expected-note@+1{{previous aliasing argument}}
  takesTwoInouts(&local, &local)
}