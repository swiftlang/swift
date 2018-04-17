// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -suppress-warnings -profile-generate -profile-coverage-mapping -emit-sorted-sil -emit-sil -module-name coverage_deinit %s | %FileCheck %s
// REQUIRES: objc_interop

import Foundation

public class Derived: NSString {
  // CHECK-LABEL: sil @$S15coverage_deinit7DerivedCfD
  // CHECK: builtin "int_instrprof_increment"
  // CHECK-NEXT: super_method {{.*}} : $Derived, #NSString.deinit!deallocator.foreign
  deinit {
  }
}

// CHECK-LABEL: sil_coverage_map "{{.*}}coverage_deinit.swift" $S15coverage_deinit7DerivedCfD
// CHECK-NEXT: [[@LINE-5]]:10 -> [[@LINE-4]]:4 : 0
