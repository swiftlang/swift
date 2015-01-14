// RUN: rm -rf %t && mkdir %t
// RUN: %build-irgen-test-overlays
// RUN: %swift -target x86_64-apple-macosx10.9 -sdk %S/Inputs -I %t %s -emit-ir | FileCheck %s

import Foundation

func foo() -> Int {
  // CHECK-LABEL: define internal i64 @_TToFC10objc_localL33_A955410181627128E3785E314285BB18_3Bar10returnFivefS0_FT_Si
  class Bar: NSObject {
    @objc func returnFive() -> Int { return 6 }
  }
  return 0
}


