// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -sdk %S/Inputs -I %S/Inputs -enable-source-import %s -enable-objc-interop | %FileCheck %s
// RUN: %target-swift-emit-sil -Xllvm -sil-print-types -O -sdk %S/Inputs -I %S/Inputs -enable-source-import %s -enable-objc-interop

import Foundation
class MyFunkyDictionary: NSDictionary {
  // CHECK-LABEL: sil hidden [ossa] @$s23super_objc_class_method17MyFunkyDictionaryC0C6MethodyyFZ : $@convention(method) (@thick MyFunkyDictionary.Type) -> ()
  // CHECK: objc_super_method %0 : $@thick MyFunkyDictionary.Type, #NSDictionary.classMethod!foreign : (NSDictionary.Type) -> () -> ()
  override class func classMethod() {
    super.classMethod()
  }
}

