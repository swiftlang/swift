// RUN: %target-swift-frontend -profile-generate -profile-coverage-mapping -emit-sil -module-name coverage_member_closure %s | FileCheck %s

class C {
  // CHECK-LABEL: sil_coverage_map {{.*}}// coverage_member_closure.C.__allocating_init
  init() {
    if (false) { // CHECK: [[@LINE]]:16 -> [[@LINE+2]]:6 : 1
      // ...
    }
  }

  // Closures in members show up at the end of the constructor's map.
  // CHECK-NOT: sil_coverage_map
  // CHECK: [[@LINE+1]]:55 -> [[@LINE+1]]:77 : 2
  var completionHandler: (String, [String]) -> Void = {(foo, bar) in return}
}
