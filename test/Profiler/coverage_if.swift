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
func foo1() -> Int {    // CHECK: [[@LINE]]:20 -> {{[0-9]+}}:2 : 0
  if .random() {        // CHECK: [[@LINE]]:6 -> [[@LINE]]:15 : 0
    return 0            // CHECK: [[@LINE-1]]:16 -> [[@LINE+2]]:4 : 1
                        // CHECK: [[@LINE+1]]:4 -> {{[0-9]+}}:2 : (0 - 1)
  } else if .random() { // CHECK: [[@LINE]]:13 -> [[@LINE]]:22 : (0 - 1)
    return 1            // CHECK: [[@LINE-1]]:23 -> [[@LINE+1]]:4 : 2
  } else {              // CHECK: [[@LINE]]:4 -> {{[0-9]+}}:2 : ((0 - 1) - 2)
    return 2            // CHECK: [[@LINE-1]]:10 -> [[@LINE+1]]:4 : ((0 - 1) - 2)
  }                     // CHECK-NOT: zero
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
func foo3() -> Int { // CHECK: [[@LINE]]:20 -> {{[0-9]+}}:2 : 0
  x: do {            // CHECK: [[@LINE]]:9 -> [[@LINE+6]]:4 : 0
    if .random() {   // CHECK: [[@LINE]]:8 -> [[@LINE]]:17 : 0
      return 0       // CHECK: [[@LINE-1]]:18 -> [[@LINE+1]]:6 : 1
    } else {         // CHECK: [[@LINE]]:6 -> [[@LINE+3]]:4 : (0 - 1)
      break x        // CHECK: [[@LINE-1]]:12 -> [[@LINE+1]]:6 : (0 - 1)
    }                // CHECK-NOT: zero
  }
  return 2
}
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
func foo5() throws -> Int { // CHECK: [[@LINE]]:27 -> {{[0-9]+}}:2 : 0
  if .random() {            // CHECK: [[@LINE]]:6 -> [[@LINE]]:15 : 0
    return 0                // CHECK: [[@LINE-1]]:16 -> [[@LINE+1]]:4 : 1
  } else {                  // CHECK: [[@LINE]]:4 -> {{[0-9]+}}:2 : (0 - 1)
    struct S: Error {}      // CHECK: [[@LINE-1]]:10 -> [[@LINE+2]]:4 : (0 - 1)
    throw S()
  }                         // CHECK-NOT: zero
}

// CHECK-LABEL: sil_coverage_map {{.*}}// coverage_if.foo6
func foo6(_ x: Int?) -> Int? { // CHECK: [[@LINE]]:30 -> {{[0-9]+}}:2 : 0
  if let x = x {
    return x                   // CHECK: [[@LINE-1]]:16 -> [[@LINE+1]]:4 : 1
  } else {                     // CHECK: [[@LINE]]:4 -> {{[0-9]+}}:2 : (0 - 1)
    return nil                 // CHECK: [[@LINE-1]]:10 -> [[@LINE+1]]:4 : (0 - 1)
  }                            // CHECK-NOT: zero
}
