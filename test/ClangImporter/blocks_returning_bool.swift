// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-silgen %s -I %S/Inputs/ | %FileCheck %s

// REQUIRES: objc_interop

import Foundation
import BlocksReturningBool

// rdar://43656704

// CHECK-LABEL: sil {{.*}} @$sSo9Aggregatea13takePredicateABySbSicSgXCSg_tcfC
// CHECK-SAME: Optional<@convention(c) (Optional<@convention(block) (Int) -> Bool>) -> ()>
func foo() -> Aggregate {
  return Aggregate(takePredicate: nil)
}
