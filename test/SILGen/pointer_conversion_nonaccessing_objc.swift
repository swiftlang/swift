// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -swift-version 4 -emit-silgen %s | %FileCheck %s

// REQUIRES: objc_interop

import Foundation

// rdar://33265254

// Check for the total absence of access markers here.
// FIXME: probably we should have some markers that just disable even static checking

var global = 0

// CHECK-LABEL: sil hidden @_T036pointer_conversion_nonaccessing_objc15testAddObserverySo8NSObjectC6object_AD8observertF
func testAddObserver(object: NSObject, observer: NSObject) {
  // CHECK: [[T0:%.*]] = global_addr @_T036pointer_conversion_nonaccessing_objc6globalSiv
  // CHECK: address_to_pointer [[T0]]
  object.addObserver(observer, forKeyPath: "", options: 0, context: &global)
}

// CHECK-LABEL: sil hidden @_T036pointer_conversion_nonaccessing_objc18testRemoveObserverySo8NSObjectC6object_AD8observertF
func testRemoveObserver(object: NSObject, observer: NSObject) {
  // CHECK: [[T0:%.*]] = global_addr @_T036pointer_conversion_nonaccessing_objc6globalSiv
  // CHECK: address_to_pointer [[T0]]
  object.removeObserver(observer, forKeyPath: "", context: &global)
}
