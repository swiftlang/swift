// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -profile-generate -profile-coverage-mapping -emit-sil -module-name coverage_member_closure %s | %FileCheck %s

class C {
  // CHECK-LABEL: sil_coverage_map {{.*}}// coverage_member_closure.C.__allocating_init
  init() {
    if (false) { // CHECK: [[@LINE]]:16 -> [[@LINE+2]]:6 : 1
      // ...
    }
  }

  // Closures in members receive their own coverage mapping.
  // CHECK: sil_coverage_map {{.*}} $S23coverage_member_closure1CC17completionHandleryySS_SaySSGtcvpfiySS_AEtcfU_
  // CHECK: [[@LINE+1]]:55 -> [[@LINE+1]]:79 : 0
  var completionHandler: (String, [String]) -> Void = { (foo, bar) in return }
}
