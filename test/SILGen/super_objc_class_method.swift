// RUN: rm -rf %t && mkdir -p %t
// RUN: %build-silgen-test-overlays

// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs -I %t) -emit-silgen %s | %FileCheck %s

// REQUIRES: objc_interop

import Foundation
class MyFunkyDictionary: NSDictionary {
  // CHECK-LABEL: sil hidden @_TZFC23super_objc_class_method17MyFunkyDictionary10initializefT_T_
  // CHECK: super_method [volatile] %0 : $@thick MyFunkyDictionary.Type, #NSObject.initialize!1.foreign : (NSObject.Type) -> () -> ()
  override class func initialize() {
    super.initialize()
  }
}

