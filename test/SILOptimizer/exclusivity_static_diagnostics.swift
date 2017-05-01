// RUN: %target-swift-frontend -enforce-exclusivity=checked -emit-sil -primary-file %s -o /dev/null -verify

import Swift

func takesTwoInouts<T>(_ p1: inout T, _ p2: inout T) { }

func simpleInoutDiagnostic() {
  var i = 7

  // expected-error@+4{{inout arguments are not allowed to alias each other}}
  // expected-note@+3{{previous aliasing argument}}
  // expected-warning@+2{{simultaneous accesses to var 'i'; modification requires exclusive access}}
  // expected-note@+1{{conflicting access is here}}
  takesTwoInouts(&i, &i)
}

func inoutOnInoutParameter(p: inout Int) {
  // expected-error@+4{{inout arguments are not allowed to alias each other}}
  // expected-note@+3{{previous aliasing argument}}
  // expected-warning@+2{{simultaneous accesses to parameter 'p'; modification requires exclusive access}}
  // expected-note@+1{{conflicting access is here}}
  takesTwoInouts(&p, &p)
}

func swapNoSuppression(_ i: Int, _ j: Int) {
  var a: [Int] = [1, 2, 3]

  // expected-warning@+2{{simultaneous accesses to var 'a'; modification requires exclusive access}}
  // expected-note@+1{{conflicting access is here}}
  swap(&a[i], &a[j]) // no-warning
}

class SomeClass { }

struct StructWithMutatingMethodThatTakesSelfInout {
  var f = SomeClass()
  mutating func mutate(_ other: inout StructWithMutatingMethodThatTakesSelfInout) { }
  mutating func mutate(_ other: inout SomeClass) { }

  mutating func callMutatingMethodThatTakesSelfInout() {
    // expected-error@+4{{inout arguments are not allowed to alias each other}}
    // expected-note@+3{{previous aliasing argument}}
    // expected-warning@+2{{simultaneous accesses to parameter 'self'; modification requires exclusive access}}
    // expected-note@+1{{conflicting access is here}}
    mutate(&self)
  }

  mutating func callMutatingMethodThatTakesSelfStoredPropInout() {
    // expected-warning@+2{{simultaneous accesses to parameter 'self'; modification requires exclusive access}}
    // expected-note@+1{{conflicting access is here}}
    mutate(&self.f)
  }
}

var globalStruct1 = StructWithMutatingMethodThatTakesSelfInout()
func callMutatingMethodThatTakesGlobalStoredPropInout() {
  // expected-warning@+2{{simultaneous accesses to var 'globalStruct1'; modification requires exclusive access}}
  // expected-note@+1{{conflicting access is here}}
  globalStruct1.mutate(&globalStruct1.f)
}

class ClassWithFinalStoredProp {
  final var s1: StructWithMutatingMethodThatTakesSelfInout = StructWithMutatingMethodThatTakesSelfInout()
  final var s2: StructWithMutatingMethodThatTakesSelfInout = StructWithMutatingMethodThatTakesSelfInout()

  func callMutatingMethodThatTakesClassStoredPropInout() {
    s1.mutate(&s2.f) // no-warning

    // expected-warning@+2{{simultaneous accesses to var 's1'; modification requires exclusive access}}
    // expected-note@+1{{conflicting access is here}}
    s1.mutate(&s1.f)

    let local1 = self

    // expected-warning@+2{{simultaneous accesses to var 's1'; modification requires exclusive access}}
    // expected-note@+1{{conflicting access is here}}
    local1.s1.mutate(&local1.s1.f)
  }
}

func violationWithGenericType<T>(_ p: T) {
  var local = p
  // expected-error@+4{{inout arguments are not allowed to alias each other}}
  // expected-note@+3{{previous aliasing argument}}
  // expected-warning@+2{{simultaneous accesses to var 'local'; modification requires exclusive access}}
  // expected-note@+1{{conflicting access is here}}
  takesTwoInouts(&local, &local)
}
