// RUN: %target-swift-frontend -enforce-exclusivity=checked -emit-sil -primary-file %s -o /dev/null -verify

import Swift

func takesTwoInouts<T>(_ p1: inout T, _ p2: inout T) { }

func simpleInoutDiagnostic() {
  var i = 7

  // expected-warning@+2{{modification requires exclusive access}}
  // expected-note@+1{{conflicting modification requires exclusive access}}
  takesTwoInouts(&i, &i)
}


func swapNoSuppression(_ i: Int, _ j: Int) {
  var a: [Int] = [1, 2, 3]

  // expected-warning@+2{{modification requires exclusive access}}
  // expected-note@+1{{conflicting modification requires exclusive access}}
  swap(&a[i], &a[j]) // no-warning
}

class SomeClass { }

struct StructWithMutatingMethodThatTakesSelfInout {
  var f = SomeClass()
  mutating func mutate(_ other: inout StructWithMutatingMethodThatTakesSelfInout) { }
  mutating func mutate(_ other: inout SomeClass) { }

  mutating func callMutatingMethodThatTakesSelfInout() {
    // expected-warning@+2{{modification requires exclusive access}}
    // expected-note@+1{{conflicting modification requires exclusive access}}
    mutate(&self)
  }

  mutating func callMutatingMethodThatTakesSelfStoredPropInout() {
    // expected-warning@+2{{modification requires exclusive access}}
    // expected-note@+1{{conflicting modification requires exclusive access}}
    mutate(&self.f)
  }
}

var global1 = StructWithMutatingMethodThatTakesSelfInout()
func callMutatingMethodThatTakesGlobalStoredPropInout() {
  // expected-warning@+2{{modification requires exclusive access}}
  // expected-note@+1{{conflicting modification requires exclusive access}}
  global1.mutate(&global1.f)
}

func violationWithGenericType<T>(_ p: T) {
  var local = p
  // expected-warning@+2{{modification requires exclusive access}}
  // expected-note@+1{{conflicting modification requires exclusive access}}
  takesTwoInouts(&local, &local)
}
