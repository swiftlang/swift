// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -profile-generate -profile-coverage-mapping -emit-sil -module-name coverage_class %s | %FileCheck %s

class C {
  // CHECK-LABEL: sil_coverage_map {{.*}}// coverage_class.C.foo
  func foo() {}
  // CHECK: sil_coverage_map {{.*}}// __ntd_C_line:[[@LINE-3]]:1
  // CHECK-NEXT: [[@LINE+1]]:10 -> [[@LINE+1]]:12
  init() {}
  // CHECK-LABEL: sil_coverage_map {{.*}}// coverage_class.C.__deallocating_deinit
  deinit {}
}

extension C {
  // CHECK-LABEL: sil_coverage_map {{.*}}// coverage_class.C.bar
  func bar() {}
}

struct S {
  // CHECK-LABEL: sil_coverage_map {{.*}}// coverage_class.S.foo
  func foo() {}
  // CHECK: sil_coverage_map {{.*}}// __ntd_S_line:[[@LINE-3]]:1
  init() {}
}

enum E {
  case X, Y, Z
  // CHECK-LABEL: sil_coverage_map {{.*}}// coverage_class.E.foo
  func foo() {}
  // CHECK: sil_coverage_map {{.*}}// __ntd_E_line:[[@LINE-4]]:1
  // CHECK-NEXT: [[@LINE+1]]:10 -> [[@LINE+1]]:23
  init() { self = .Y }
}

var g1: Bool = true

struct S2 {
  // CHECK: sil_coverage_map {{.*}}// __ntd_S2_line:[[@LINE-1]]:1
  // CHECK-NEXT: [[@LINE+2]]:22 -> [[@LINE+2]]:23 : 0
  // CHECK-NEXT: [[@LINE+1]]:26 -> [[@LINE+1]]:27 : (1 - 0)
  var m1: Int = g1 ? 0 : 1
}

// Test that the crash from SR-8429 is avoided. Follow-up work is
// needed to generate the correct coverage mapping here. Coverage for
// `offset` should be associated with its getter, not the class
// constructor.
class C2 {
  lazy var offset: Int = true ? 30 : 55
}
