// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -target x86_64-apple-darwin10 -module-cache-path %t/clang-module-cache -sdk %S/Inputs -I=%S/Inputs -enable-source-import %s -emit-ir | FileCheck %s

import Foundation

func foo() -> Int {
  // CHECK-LABEL: define linkonce_odr hidden i64 @_TToFCF10objc_local3fooFT_SiL_3Bar10returnFivefS0_FT_Si
  class Bar: NSObject {
    func returnFive() -> Int { return 6 }
  }
  return 0
}


