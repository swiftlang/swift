// RUN: %target-swift-emit-silgen(mock-sdk: %clang-importer-sdk) %s | %FileCheck %s
// REQUIRES: objc_interop

// rdar://problem/30030229

import Foundation

class MyDictionary: NSDictionary {
  // CHECK-LABEL: sil hidden @$s4main12MyDictionaryC31callSuperNonObjCExtensionMethodyySiF
  func callSuperNonObjCExtensionMethod(_ x: Int) {
    // CHECK-NOT: super_method {{.*}} #NSDictionary.nonObjCExtensionMethod
    super.nonObjCExtensionMethod(x)
  }
}
