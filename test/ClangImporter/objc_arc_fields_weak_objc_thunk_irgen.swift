// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-ir -enable-experimental-feature ImportCStructsWithArcFields %s | %FileCheck %s

// REQUIRES: objc_interop
// REQUIRES: swift_feature_ImportCStructsWithArcFields

import Foundation
import objc_structs

class WeakArcStructSubclass: WeakArcStructBase {
  override func transformWeakArcStruct(
    _ value: WeaksInAStructArc
  ) -> WeaksInAStructArc {
    return value
  }
}

// An address-only C struct uses both an indirect result and an indirect
// parameter in the Objective-C entry point. The thunk must index its formal
// parameters after the indirect result, including self.
// CHECK-LABEL: define internal void @"$s{{.*}}WeakArcStructSubclassC{{.*}}transform{{.*}}To"
// CHECK-SAME: ptr noalias sret(%TSo17WeaksInAStructArcV) %0, ptr %1, ptr %2, ptr %3
// CHECK: call swiftcc void @"$s{{.*}}WeakArcStructSubclassC{{.*}}transform{{.*}}"(
// CHECK-SAME: ptr noalias sret(%TSo17WeaksInAStructArcV) %0,
// CHECK-SAME: ptr noalias{{.*}}dereferenceable(8) %3,
// CHECK-SAME: ptr swiftself %1)
