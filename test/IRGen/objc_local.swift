// RUN: %empty-directory(%t)
// RUN: %build-irgen-test-overlays
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs -I %t) %s -emit-ir | %FileCheck %s

// REQUIRES: objc_interop

import Foundation

func foo() -> Int64 {
  // CHECK-LABEL: define internal i64 @"$s10objc_local3foos5Int64VyF3BarL_C10returnFiveADyFTo"
  class Bar: NSObject {
    @objc func returnFive() -> Int64 { return 6 }
  }
  return 0
}


