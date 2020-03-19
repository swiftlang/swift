// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -enable-testing -profile-generate -profile-coverage-mapping -emit-sil -module-name coverage_subscript_autoclosure %s | %FileCheck %s
// RUN: %target-swift-frontend -profile-generate -profile-coverage-mapping -emit-ir %s

struct S {
  subscript(i: Int, autoclosure: @autoclosure () ->  Int) -> Int {
    // CHECK-LABEL: sil_coverage_map {{.*}}S.subscript.getter
    get { // CHECK-NEXT: [[@LINE]]:9 -> [[@LINE+2]]:6 : 0
      return 0
    }

    // CHECK-LABEL: sil_coverage_map {{.*}}S.subscript.setter
    set { // CHECK-NEXT: [[@LINE]]:9 -> [[@LINE+2]]:6 : 0

    }
  }
}
