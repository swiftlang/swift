// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-silgen %s | %FileCheck %s
// REQUIRES: objc_interop

// rdar://problem/30030229

import Foundation

class MyDictionary: NSDictionary {
  // CHECK-LABEL: sil hidden @_TFC4main12MyDictionary31callSuperNonObjCExtensionMethodfSiT_
  func callSuperNonObjCExtensionMethod(_ x: Int) {
    // CHECK-NOT: super_method {{.*}} #NSDictionary.nonObjCExtensionMethod
    super.nonObjCExtensionMethod(x)
  }
}
