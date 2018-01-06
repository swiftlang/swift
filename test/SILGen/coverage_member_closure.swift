// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -profile-generate -profile-coverage-mapping -emit-sil -module-name coverage_member_closure %s | %FileCheck %s

class C {
  // CHECK-LABEL: sil_coverage_map {{.*}}// coverage_member_closure.C.__allocating_init
  init() {
    if (false) { // CHECK: [[@LINE]]:16 -> [[@LINE+2]]:6 : 1
      // ...
    }
  }

  // Closures in members receive their own coverage mapping.
  // Note: Don't use the full mangled name here, because the mangling changes after 4.1.
  // CHECK: sil_coverage_map {{.*}} _T023coverage_member_closure1CC17completionHandler
  // CHECK: [[@LINE+1]]:55 -> [[@LINE+1]]:79 : 0
  var completionHandler: (String, [String]) -> Void = { (foo, bar) in return }
}
