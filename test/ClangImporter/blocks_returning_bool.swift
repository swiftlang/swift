// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-silgen %s -I %S/Inputs/ | %FileCheck %s

// REQUIRES: objc_interop

import Foundation
import BlocksReturningBool

// rdar://43656704

// CHECK-LABEL: sil {{.*}} @$sSo9Aggregatea13takePredicateABySbSicSgXzC28_ZTSPFvU13block_pointerFbmEESg_tcfC
// CHECK-SAME: Optional<@convention(c, cType: "void (*)(_Bool (^)(unsigned long))") (Optional<@convention(block) (Int) -> Bool>) -> ()>
func foo() -> Aggregate {
  return Aggregate(takePredicate: nil)
}
