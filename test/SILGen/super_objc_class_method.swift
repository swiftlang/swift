// RUN: %target-swift-emit-silgen -sdk %S/Inputs -I %S/Inputs -enable-source-import %s -enable-objc-interop -swift-version 3 | %FileCheck %s

import Foundation
class MyFunkyDictionary: NSDictionary {
  // CHECK-LABEL: sil hidden @$S23super_objc_class_method17MyFunkyDictionaryC10initializeyyFZ
  // CHECK: objc_super_method %0 : $@thick MyFunkyDictionary.Type, #NSObject.initialize!1.foreign : (NSObject.Type) -> () -> ()
  override class func initialize() {
    super.initialize()
  }
}

