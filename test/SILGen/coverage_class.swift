// RUN: %target-swift-frontend -profile-generate -profile-coverage-mapping -emit-sil -module-name coverage_class %s | FileCheck %s

class C {
  // CHECK-LABEL: sil_coverage_map {{.*}}// coverage_class.C.foo
  func foo() {}
  // CHECK-LABEL: sil_coverage_map {{.*}}// coverage_class.C.__allocating_init
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
  // CHECK-LABEL: sil_coverage_map {{.*}}// coverage_class.S.init
  init() {}
}

enum E {
  case X, Y, Z
  // CHECK-LABEL: sil_coverage_map {{.*}}// coverage_class.E.foo
  func foo() {}
  // CHECK-LABEL: sil_coverage_map {{.*}}// coverage_class.E.init
  init() { self = Y }
}
