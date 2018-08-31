// RUN: %target-swift-emit-silgen(mock-sdk: %clang-importer-sdk) -swift-version 4 -enable-sil-ownership %s | %FileCheck %s

// REQUIRES: objc_interop

import Foundation

// rdar://33265254

// Check for the total absence of access markers here.
// FIXME: probably we should have some markers that just disable even static checking

var global = 0

// CHECK-LABEL: sil hidden @$S36pointer_conversion_nonaccessing_objc15testAddObserver6object8observerySo8NSObjectC_AFtF
func testAddObserver(object: NSObject, observer: NSObject) {
  // CHECK: [[T0:%.*]] = global_addr @$S36pointer_conversion_nonaccessing_objc6globalSiv
  // CHECK: address_to_pointer [[T0]] :
  object.addObserver(observer, forKeyPath: "", options: 0, context: &global)
}

// CHECK-LABEL: sil hidden @$S36pointer_conversion_nonaccessing_objc18testRemoveObserver6object8observerySo8NSObjectC_AFtF
func testRemoveObserver(object: NSObject, observer: NSObject) {
  // CHECK: [[T0:%.*]] = global_addr @$S36pointer_conversion_nonaccessing_objc6globalSiv
  // CHECK: address_to_pointer [[T0]] :
  object.removeObserver(observer, forKeyPath: "", context: &global)
}

// rdar://33850465
//   Make sure this applies to AnyObject dispatch, too.

// CHECK-LABEL: sil hidden @$S36pointer_conversion_nonaccessing_objc28testDynamicForcedAddObserver6object8observeryyXl_So8NSObjectCtF
func testDynamicForcedAddObserver(object: AnyObject, observer: NSObject) {
  // CHECK: [[T0:%.*]] = global_addr @$S36pointer_conversion_nonaccessing_objc6globalSiv
  // CHECK: address_to_pointer [[T0]] :
  object.addObserver!(observer, forKeyPath: "", options: 0, context: &global)
}

// CHECK-LABEL: sil hidden @$S36pointer_conversion_nonaccessing_objc31testDynamicForcedRemoveObserver6object8observeryyXl_So8NSObjectCtF
func testDynamicForcedRemoveObserver(object: AnyObject, observer: NSObject) {
  // CHECK: [[T0:%.*]] = global_addr @$S36pointer_conversion_nonaccessing_objc6globalSiv
  // CHECK: address_to_pointer [[T0]] :
  object.removeObserver!(observer, forKeyPath: "", context: &global)
}

// CHECK-LABEL: sil hidden @$S36pointer_conversion_nonaccessing_objc30testDynamicOptionalAddObserver6object8observeryyXl_So8NSObjectCtF
func testDynamicOptionalAddObserver(object: AnyObject, observer: NSObject) {
  // CHECK: [[T0:%.*]] = global_addr @$S36pointer_conversion_nonaccessing_objc6globalSiv
  // CHECK: address_to_pointer [[T0]] :
  object.addObserver?(observer, forKeyPath: "", options: 0, context: &global)
}

// CHECK-LABEL: sil hidden @$S36pointer_conversion_nonaccessing_objc33testDynamicOptionalRemoveObserver6object8observeryyXl_So8NSObjectCtF
func testDynamicOptionalRemoveObserver(object: AnyObject, observer: NSObject) {
  // CHECK: [[T0:%.*]] = global_addr @$S36pointer_conversion_nonaccessing_objc6globalSiv
  // CHECK: address_to_pointer [[T0]] :
  object.removeObserver?(observer, forKeyPath: "", context: &global)
}
