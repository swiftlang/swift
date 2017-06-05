// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-silgen %s | %FileCheck %s
// REQUIRES: objc_interop

import Foundation

protocol Fooable {}

extension Fooable where Self: _ObjectiveCBridgeable {
  func _bridgeToObjectiveC() -> _ObjectiveCType {
    fatalError()
  }

  static func _forceBridgeFromObjectiveC(
    _ source: _ObjectiveCType,
    result: inout Self?
  ) {
    fatalError()
  }

  static func _conditionallyBridgeFromObjectiveC(
    _ source: _ObjectiveCType,
    result: inout Self?
  ) -> Bool {
    fatalError()
  }

  static func _unconditionallyBridgeFromObjectiveC(_ source: _ObjectiveCType?)
      -> Self {
    fatalError()
  }
}

struct Foo: Fooable, _ObjectiveCBridgeable {
  typealias _ObjectiveCType = NSObject
}
struct Gen<T, U>: Fooable, _ObjectiveCBridgeable {
  typealias _ObjectiveCType = NSObject
}

class Bar: NSObject {
  dynamic func bar(_: Any) {}
}

// CHECK-LABEL: sil hidden @_T042objc_bridged_using_protocol_extension_impl7callBaryAA0H0C3bar_AA3FooV3footF
func callBar(bar: Bar, foo: Foo) {
  // CHECK: [[BRIDGE:%.*]] = function_ref @_T042objc_bridged_using_protocol_extension_impl7FooablePAAs21_ObjectiveCBridgeableRzAaBRzlE09_bridgeToH1C01_H5CTypesADPQzyF
  // CHECK: apply [[BRIDGE]]<Foo>
  bar.bar(foo)
}

// CHECK-LABEL:sil hidden @_T042objc_bridged_using_protocol_extension_impl7callBaryAA0H0C3bar_AA3GenVySiSSG3gentF 
func callBar(bar: Bar, gen: Gen<Int, String>) {
  // CHECK: [[BRIDGE:%.*]] = function_ref @_T042objc_bridged_using_protocol_extension_impl7FooablePAAs21_ObjectiveCBridgeableRzAaBRzlE09_bridgeToH1C01_H5CTypesADPQzyF
  // CHECK: apply [[BRIDGE]]<Gen<Int, String>>
  bar.bar(gen)
}
