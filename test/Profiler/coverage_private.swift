// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -profile-generate -profile-coverage-mapping -emit-sorted-sil -emit-sil -module-name coverage_private %s | %FileCheck %s
// RUN: %target-swift-frontend -profile-generate -profile-coverage-mapping -emit-ir %s

struct S {
  func visible() {
    hidden()
  }

  // CHECK-LABEL: sil_coverage_map {{.*}}// coverage_private.S.(hidden in {{.*}})() -> ()
  // CHECK-NEXT:  [[@LINE+1]]:25 -> [[@LINE+2]]:4 : 0
  private func hidden() {
  }
}

S().visible()
