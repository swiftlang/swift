// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -profile-generate -profile-coverage-mapping -emit-sorted-sil -emit-sil -module-name coverage_if %s | %FileCheck %s
// RUN: %target-swift-frontend -profile-generate -profile-coverage-mapping -emit-ir %s

// CHECK-LABEL: sil_coverage_map {{.*}}// coverage_if.foo
func foo(x : Bool) { // CHECK: [[@LINE]]:20 -> {{[0-9]+}}:2 : 0
           // CHECK: [[@LINE+1]]:6 -> [[@LINE+1]]:9 : 0
  if (x) { // CHECK: [[@LINE]]:10 -> [[@LINE+1]]:4 : 1
  }

  if (!x) { // CHECK: [[@LINE]]:11 -> [[@LINE+2]]:4 : 2
    // ...
  }

  if (x) { } // CHECK: [[@LINE]]:10 -> [[@LINE]]:13 : 3
  else   { } // CHECK: [[@LINE]]:10 -> [[@LINE]]:13 : (0 - 3)
}

foo(x: true);
foo(x: false);

// rdar://29390569 – Make sure we don't add a spurious unreachable empty region.

// CHECK-LABEL: sil_coverage_map {{.*}}// coverage_if.foo1
func foo1() -> Int {    // CHECK-NEXT: [[@LINE]]:20   -> {{[0-9]+}}:2  : 0
  if .random() {        // CHECK-NEXT: [[@LINE]]:6    -> [[@LINE]]:15  : 0
    return 0            // CHECK-NEXT: [[@LINE-1]]:16 -> [[@LINE+1]]:4 : 1
  } else if .random() { // CHECK-NEXT: [[@LINE]]:13   -> [[@LINE]]:22  : (0 - 1)
    return 1            // CHECK-NEXT: [[@LINE-1]]:23 -> [[@LINE+1]]:4 : 2
  } else {              // CHECK-NEXT: [[@LINE]]:10   -> [[@LINE+2]]:4 : ((0 - 1) - 2)
    return 2
  }                     // CHECK-NEXT: }
}

// ...but we will add an unreachable region if you write code there.
// CHECK-LABEL: sil_coverage_map {{.*}}// coverage_if.foo2
func foo2() -> Int {
  if .random() {
    return 0
  } else if .random() {
    return 1
  } else {
    return 2
  }
  _ = foo1() // CHECK: [[@LINE]]:3 -> [[@LINE+1]]:2 : zero
}

// Make sure we don't add unreachable regions for a labeled jump either.
// CHECK-LABEL: sil_coverage_map {{.*}}// coverage_if.foo3
func foo3() -> Int { // CHECK-NEXT: [[@LINE]]:20   -> {{[0-9]+}}:2   : 0
  x: do {            // CHECK-NEXT: [[@LINE]]:9    -> [[@LINE+6]]:4  : 0
    if .random() {   // CHECK-NEXT: [[@LINE]]:8    -> [[@LINE]]:17   : 0
      return 0       // CHECK-NEXT: [[@LINE-1]]:18 -> [[@LINE+1]]:6  : 1
    } else {         // CHECK-NEXT: [[@LINE]]:12   -> [[@LINE+2]]:6  : (0 - 1)
      break x
    }
  }                  // CHECK-NEXT: [[@LINE]]:4    -> [[@LINE+1]]:11 : (0 - 1)
  return 2
}                    // CHECK-NEXT: }

// CHECK-LABEL: sil_coverage_map {{.*}}// coverage_if.foo4
func foo4() -> Int {
  x: do {
    if .random() {
      return 0
    } else {
      break x
    }
    let y = 0 // CHECK: [[@LINE]]:13 -> [[@LINE+1]]:4 : zero
  }
  return 2
}

// CHECK-LABEL: sil_coverage_map {{.*}}// coverage_if.foo5
func foo5() throws -> Int { // CHECK-NEXT: [[@LINE]]:27   -> {{[0-9]+}}:2  : 0
  if .random() {            // CHECK-NEXT: [[@LINE]]:6    -> [[@LINE]]:15  : 0
    return 0                // CHECK-NEXT: [[@LINE-1]]:16 -> [[@LINE+1]]:4 : 1
  } else {                  // CHECK-NEXT: [[@LINE]]:10   -> [[@LINE+3]]:4 : (0 - 1)
    struct S: Error {}
    throw S()
  }
}                           // CHECK-NEXT: }

// CHECK-LABEL: sil_coverage_map {{.*}}// coverage_if.foo6
func foo6(_ x: Int?) -> Int? { // CHECK-NEXT: [[@LINE]]:30 -> {{[0-9]+}}:2  : 0
  if let x = x {               // CHECK-NEXT: [[@LINE]]:16 -> [[@LINE+2]]:4 : 1
    return x
  } else {                     // CHECK-NEXT: [[@LINE]]:10 -> [[@LINE+2]]:4 : (0 - 1)
    return nil
  }
}                              // CHECK-NEXT: }

// rdar://104078910, rdar://104079242 - Make sure the else if condition and the
// else gets a correct counter here.
// CHECK-LABEL: sil_coverage_map {{.*}}// coverage_if.foo7
func foo7() -> Int {    // CHECK-NEXT: [[@LINE]]:20   -> [[@LINE+12]]:2 : 0
  let x: Int
  if .random() {        // CHECK-NEXT: [[@LINE]]:6    -> [[@LINE]]:15   : 0
    x = 0               // CHECK-NEXT: [[@LINE-1]]:16 -> [[@LINE+1]]:4  : 1
  } else if .random() { // CHECK-NEXT: [[@LINE]]:13   -> [[@LINE]]:22   : (0 - 1)
    x = 1               // CHECK-NEXT: [[@LINE-1]]:23 -> [[@LINE+1]]:4  : 2
  } else if .random() { // CHECK-NEXT: [[@LINE]]:13   -> [[@LINE]]:22   : ((0 - 1) - 2)
    x = 2               // CHECK-NEXT: [[@LINE-1]]:23 -> [[@LINE+1]]:4  : 3
  } else {              // CHECK-NEXT: [[@LINE]]:10   -> [[@LINE+2]]:4  : (((0 - 1) - 2) - 3)
    x = 3
  }
  return x
}                       // CHECK-NEXT: }

// CHECK-LABEL: sil_coverage_map {{.*}}// coverage_if.foo8
func foo8() -> Int {    // CHECK-NEXT: [[@LINE]]:20   -> [[@LINE+12]]:2 : 0
  let x: Int            // CHECK-NEXT: [[@LINE+1]]:6  -> [[@LINE+1]]:15 : 0
  if .random() {        // CHECK-NEXT: [[@LINE]]:16   -> [[@LINE+2]]:4  : 1
    return 0
  } else if .random() { // CHECK-NEXT: [[@LINE]]:13   -> [[@LINE]]:22   : (0 - 1)
    x = 1               // CHECK-NEXT: [[@LINE-1]]:23 -> [[@LINE+1]]:4  : 2
  } else if .random() { // CHECK-NEXT: [[@LINE]]:13   -> [[@LINE]]:22   : ((0 - 1) - 2)
    x = 2               // CHECK-NEXT: [[@LINE-1]]:23 -> [[@LINE+1]]:4  : 3
  } else {              // CHECK-NEXT: [[@LINE]]:10   -> [[@LINE+2]]:4  : (((0 - 1) - 2) - 3)
    x = 3
  }                     // CHECK-NEXT: [[@LINE]]:4    -> [[@LINE+1]]:11 : (0 - 1)
  return x
}                       // CHECK-NEXT: }

// CHECK-LABEL: sil_coverage_map {{.*}}// coverage_if.foo9
func foo9() -> Int {    // CHECK-NEXT: [[@LINE]]:20   -> [[@LINE+12]]:2 : 0
  let x: Int            // CHECK-NEXT: [[@LINE+1]]:6  -> [[@LINE+1]]:15 : 0
  if .random() {        // CHECK-NEXT: [[@LINE]]:16   -> [[@LINE+2]]:4  : 1
    return 0
  } else if .random() { // CHECK-NEXT: [[@LINE]]:13   -> [[@LINE]]:22   : (0 - 1)
    return 1            // CHECK-NEXT: [[@LINE-1]]:23 -> [[@LINE+1]]:4  : 2
  } else if .random() { // CHECK-NEXT: [[@LINE]]:13   -> [[@LINE]]:22   : ((0 - 1) - 2)
    x = 2               // CHECK-NEXT: [[@LINE-1]]:23 -> [[@LINE+1]]:4  : 3
  } else {              // CHECK-NEXT: [[@LINE]]:10   -> [[@LINE+2]]:4  : (((0 - 1) - 2) - 3)
    x = 3
  }                     // CHECK-NEXT: [[@LINE]]:4    -> [[@LINE+1]]:11 : ((0 - 1) - 2)
  return x
}                       // CHECK-NEXT: }

// CHECK-LABEL: sil_coverage_map {{.*}}// coverage_if.foo10
func foo10() -> Int {   // CHECK-NEXT: [[@LINE]]:21   -> [[@LINE+12]]:2 : 0
  let x: Int            // CHECK-NEXT: [[@LINE+1]]:6  -> [[@LINE+1]]:15 : 0
  if .random() {        // CHECK-NEXT: [[@LINE]]:16   -> [[@LINE+2]]:4  : 1
    return 0
  } else if .random() { // CHECK-NEXT: [[@LINE]]:13   -> [[@LINE]]:22   : (0 - 1)
    return 1            // CHECK-NEXT: [[@LINE-1]]:23 -> [[@LINE+1]]:4  : 2
  } else if .random() { // CHECK-NEXT: [[@LINE]]:13   -> [[@LINE]]:22   : ((0 - 1) - 2)
    x = 2               // CHECK-NEXT: [[@LINE-1]]:23 -> [[@LINE+1]]:4  : 3
  } else {              // CHECK-NEXT: [[@LINE]]:10   -> [[@LINE+2]]:4  : (((0 - 1) - 2) - 3)
    return 3
  }                     // CHECK-NEXT: [[@LINE]]:4    -> [[@LINE+1]]:11 : 3
  return x
}                       // CHECK-NEXT: }

// CHECK-LABEL: sil_coverage_map {{.*}}// coverage_if.foo11
func foo11() -> Int {   // CHECK-NEXT: [[@LINE]]:21   -> [[@LINE+12]]:2 : 0
  let x: Int            // CHECK-NEXT: [[@LINE+1]]:6  -> [[@LINE+1]]:15 : 0
  if .random() {        // CHECK-NEXT: [[@LINE]]:16   -> [[@LINE+2]]:4  : 1
    x = 0
  } else if .random() { // CHECK-NEXT: [[@LINE]]:13   -> [[@LINE]]:22   : (0 - 1)
    x = 1               // CHECK-NEXT: [[@LINE-1]]:23 -> [[@LINE+1]]:4  : 2
  } else if .random() { // CHECK-NEXT: [[@LINE]]:13   -> [[@LINE]]:22   : ((0 - 1) - 2)
    x = 2               // CHECK-NEXT: [[@LINE-1]]:23 -> [[@LINE+1]]:4  : 3
  } else {              // CHECK-NEXT: [[@LINE]]:10   -> [[@LINE+2]]:4  : (((0 - 1) - 2) - 3)
    return 3
  }                     // CHECK-NEXT: [[@LINE]]:4    -> [[@LINE+1]]:11 : ((1 + 2) + 3)
  return x
}                       // CHECK-NEXT: }

// CHECK-LABEL: sil_coverage_map {{.*}}// coverage_if.foo12
func foo12() -> Int { // CHECK-NEXT: [[@LINE]]:21   -> [[@LINE+9]]:2  : 0
  let x: Int
  y: if .random() {   // CHECK-NEXT: [[@LINE]]:9    -> [[@LINE]]:18   : 0
    x = 0             // CHECK-NEXT: [[@LINE-1]]:19 -> [[@LINE+2]]:4  : 1
    break y
  } else {
    x = 1             // CHECK-NEXT: [[@LINE-1]]:10 -> [[@LINE+1]]:4  : (0 - 1)
  }
  return x
}                     // CHECK-NEXT: }

// CHECK-LABEL: sil_coverage_map {{.*}}// coverage_if.foo13
func foo13() -> Int { // CHECK-NEXT: [[@LINE]]:21   -> [[@LINE+10]]:2 : 0
  let x: Int
  y: if .random() {   // CHECK-NEXT: [[@LINE]]:9    -> [[@LINE]]:18   : 0
    x = 0             // CHECK-NEXT: [[@LINE-1]]:19 -> [[@LINE+2]]:4  : 1
    break y
  } else {
    x = 1             // CHECK-NEXT: [[@LINE-1]]:10 -> [[@LINE+2]]:4  : (0 - 1)
    break y
  }
  return x
}                     // CHECK-NEXT: }

// CHECK-LABEL: sil_coverage_map {{.*}}// coverage_if.foo14
func foo14() -> Int {   // CHECK-NEXT: [[@LINE]]:21   -> [[@LINE+14]]:2 : 0
  let x: Int            // CHECK-NEXT: [[@LINE+1]]:9  -> [[@LINE+1]]:18 : 0
  y: if .random() {     // CHECK-NEXT: [[@LINE]]:19   -> [[@LINE+3]]:4  : 1
    x = 0
    break y
  } else if .random() { // CHECK-NEXT: [[@LINE]]:13   -> [[@LINE]]:22   : (0 - 1)
    return 1            // CHECK-NEXT: [[@LINE-1]]:23 -> [[@LINE+1]]:4  : 2
  } else if .random() { // CHECK-NEXT: [[@LINE]]:13   -> [[@LINE]]:22   : ((0 - 1) - 2)
    x = 2               // CHECK-NEXT: [[@LINE-1]]:23 -> [[@LINE+2]]:4  : 3
    break y
  } else {              // CHECK-NEXT: [[@LINE]]:10   -> [[@LINE+2]]:4  : (((0 - 1) - 2) - 3)
    return 3
  }                     // CHECK-NEXT: [[@LINE]]:4    -> [[@LINE+1]]:11 : (1 + 3)
  return x
}                       // CHECK-NEXT: }

// CHECK-LABEL: sil_coverage_map {{.*}}// coverage_if.foo15
func foo15() -> Int {   // CHECK-NEXT: [[@LINE]]:21   -> [[@LINE+19]]:2 : 0
  let x: Int            // CHECK-NEXT: [[@LINE+1]]:9  -> [[@LINE+1]]:18 : 0
  y: if .random() {     // CHECK-NEXT: [[@LINE]]:19   -> [[@LINE+5]]:4  : 1
    x = 0
    if .random() {      // CHECK-NEXT: [[@LINE]]:8    -> [[@LINE]]:17   : 1
      break y           // CHECK-NEXT: [[@LINE-1]]:18 -> [[@LINE+1]]:6  : 2
    }                   // CHECK-NEXT: [[@LINE]]:6    -> [[@LINE+1]]:4  : (1 - 2)
  } else if .random() { // CHECK-NEXT: [[@LINE]]:13   -> [[@LINE]]:22   : (0 - 1)
    x = 1               // CHECK-NEXT: [[@LINE-1]]:23 -> [[@LINE+4]]:4  : 3
    if .random() {      // CHECK-NEXT: [[@LINE]]:8    -> [[@LINE]]:17   : 3
      return 1          // CHECK-NEXT: [[@LINE-1]]:18 -> [[@LINE+1]]:6  : 4
    }                   // CHECK-NEXT: [[@LINE]]:6    -> [[@LINE+1]]:4  : (3 - 4)
  } else if .random() { // CHECK-NEXT: [[@LINE]]:13   -> [[@LINE]]:22   : ((0 - 1) - 3)
    x = 2               // CHECK-NEXT: [[@LINE-1]]:23 -> [[@LINE+2]]:4  : 5
    break y
  } else {              // CHECK-NEXT: [[@LINE]]:10   -> [[@LINE+2]]:4  : (((0 - 1) - 3) - 5)
    return 3
  }                     // CHECK-NEXT: [[@LINE]]:4    -> [[@LINE+1]]:11 : (((1 + 3) + 5) - 4)
  return x
}                       // CHECK-NEXT: }
