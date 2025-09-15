// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -suppress-warnings -profile-generate -profile-coverage-mapping -emit-sorted-sil -Xllvm -sil-print-types -emit-sil -module-name coverage_deinit %s | %FileCheck %s
// RUN: %target-swift-frontend -profile-generate -profile-coverage-mapping -emit-ir %s
// REQUIRES: objc_interop

import Foundation

public class Derived: NSString {
  // CHECK-LABEL: sil @$s15coverage_deinit7DerivedCfD
  // CHECK: increment_profiler_counter 0
  // CHECK-NEXT: super_method {{.*}} : $Derived, #NSString.deinit!deallocator.foreign
  deinit {
  }
}

// CHECK-LABEL: sil_coverage_map "{{.*}}coverage_deinit_objc.swift" "$s15coverage_deinit7DerivedCfD"
// CHECK-NEXT: [[@LINE-5]]:10 -> [[@LINE-4]]:4 : 0
