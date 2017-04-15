// RUN: %target-swift-frontend -enforce-exclusivity=checked -emit-sil -primary-file %s -o /dev/null -verify

import Swift

func takesTwoInouts(_ p1: inout Int, _ p2: inout Int) { }

func simpleInoutDiagnostic() {
  var i = 7

  // expected-warning@+2{{modification requires exclusive access}}
  // expected-note@+1{{conflicting modification requires exclusive access}}
  takesTwoInouts(&i, &i)
}


func swapSuppression(_ i: Int, _ j: Int) {
  var a: [Int] = [1, 2, 3]

  swap(&a[i], &a[j]) // no-warning

  // expected-warning@+2{{modification requires exclusive access}}
  // expected-note@+1{{conflicting modification requires exclusive access}}
  takesTwoInouts(&a[i], &a[j])
}

func missedSwapSuppression(_ i: Int, _ j: Int) {
  var a: [Int] = [1, 2, 3]

  // We don't suppress when swap() is used as a value
  let mySwap: (inout Int, inout Int) -> () = swap

  // expected-warning@+2{{modification requires exclusive access}}
  // expected-note@+1{{conflicting modification requires exclusive access}}
  mySwap(&a[i], &a[j])
}

func dontSuppressUserSwap(_ i: Int, _ j: Int) {
  var a: [Int] = [1, 2, 3]

  // Don't suppress on user-defined swap.
  func swap<T>(_ p1: inout T, _ p2: inout T) {
    return (p1, p2) = (p2, p1)
  }

  // expected-warning@+2{{modification requires exclusive access}}
  // expected-note@+1{{conflicting modification requires exclusive access}}
  swap(&a[i], &a[j])
}
