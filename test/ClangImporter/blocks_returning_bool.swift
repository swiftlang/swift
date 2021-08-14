// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-silgen %s -I %S/Inputs/ | %FileCheck %s

// Check that bridging an @convention(c)'s function's @convention(block)
// parameter to a Swift function type doesn't crash the compiler.
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-silgen %s -I %S/Inputs/ -o /dev/null -use-clang-function-types -experimental-print-full-convention

// REQUIRES: objc_interop

import Foundation
import BlocksReturningBool

// rdar://43656704

// CHECK-LABEL: sil {{.*}} @$sSo9Aggregatea13takePredicateABySbSicSgXCSg_tcfC
// CHECK-SAME: Optional<@convention(c) (Optional<@convention(block) (Int) -> Bool>) -> ()>
func foo() -> Aggregate {
  return Aggregate(takePredicate: nil)
}
