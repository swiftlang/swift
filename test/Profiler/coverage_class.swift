// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -profile-generate -profile-coverage-mapping -emit-sil -module-name coverage_class %s | %FileCheck %s
// RUN: %target-swift-frontend -profile-generate -profile-coverage-mapping -emit-ir %s

class C {
  // CHECK-LABEL: sil_coverage_map {{.*}}// coverage_class.C.foo() -> ()
  // CHECK-NEXT: [[@LINE+1]]:14 -> [[@LINE+1]]:16 : 0
  func foo() {}

  // CHECK: sil_coverage_map {{.*}}// coverage_class.C.init() -> coverage_class.C
  // CHECK-NEXT: [[@LINE+1]]:10 -> [[@LINE+1]]:12
  init() {}

  // CHECK-LABEL: sil_coverage_map {{.*}}// coverage_class.C.__deallocating_deinit
  // CHECK-NEXT: [[@LINE+1]]:10 -> [[@LINE+1]]:12
  deinit {}
}

extension C {
  // CHECK-LABEL: sil_coverage_map {{.*}}// coverage_class.C.bar() -> ()
  // CHECK-NEXT: [[@LINE+1]]:14 -> [[@LINE+1]]:16 : 0
  func bar() {}
}

struct S {
  // CHECK-LABEL: sil_coverage_map {{.*}}// coverage_class.S.foo() -> ()
  // CHECK-NEXT: [[@LINE+1]]:14 -> [[@LINE+1]]:16 : 0
  func foo() {}

  // CHECK: sil_coverage_map {{.*}}// coverage_class.S.init() -> coverage_class.S
  // CHECK-NEXT: [[@LINE+1]]:10 -> [[@LINE+1]]:12
  init() {}
}

enum E {
  case X, Y, Z

  // CHECK-LABEL: sil_coverage_map {{.*}}// coverage_class.E.foo() -> ()
  // CHECK-NEXT: [[@LINE+1]]:14 -> [[@LINE+1]]:16 : 0
  func foo() {}

  // CHECK: sil_coverage_map {{.*}}// coverage_class.E.init() -> coverage_class.E
  // CHECK-NEXT: [[@LINE+1]]:10 -> [[@LINE+1]]:23
  init() { self = .Y }
}

var g1: Bool = true

struct S2 {
  // CHECK: sil_coverage_map {{.*}}// variable initialization expression of coverage_class.S2.m1 : Swift.Int
  // CHECK-NEXT: [[@LINE+3]]:22 -> [[@LINE+3]]:23 : 1
  // CHECK-NEXT: [[@LINE+2]]:26 -> [[@LINE+2]]:27 : (0 - 1)
  // CHECK-NEXT: [[@LINE+1]]:17 -> [[@LINE+1]]:27 : 0
  var m1: Int = g1 ? 0 : 1
}

// Test that the crash from SR-8429 is avoided. Follow-up work is
// needed to generate the correct coverage mapping here. Coverage for
// `offset` should be associated with its getter, not the class
// constructor.
class C2 {
  lazy var offset: Int = true ? 30 : 55
}
