// RUN: %target-swift-frontend -enforce-exclusivity=checked -emit-sil -primary-file %s -o /dev/null -verify

import Swift

func takesTwoInouts(_ p1: inout Int, _ p2: inout Int) { }

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
