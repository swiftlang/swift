// RUN: %target-swift-emit-silgen(mock-sdk: %clang-importer-sdk) -enable-sil-ownership %s | %FileCheck %s
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
  @objc dynamic func bar(_: Any) {}
}

// CHECK-LABEL: sil hidden @$s42objc_bridged_using_protocol_extension_impl7callBar3bar3fooyAA0H0C_AA3FooVtF
func callBar(bar: Bar, foo: Foo) {
  // CHECK: [[BRIDGE:%.*]] = function_ref @$s42objc_bridged_using_protocol_extension_impl7FooablePAAs21_ObjectiveCBridgeableRzrlE09_bridgeToH1C01_H5CTypesADPQzyF
  // CHECK: apply [[BRIDGE]]<Foo>
  bar.bar(foo)
}

// CHECK-LABEL:sil hidden @$s42objc_bridged_using_protocol_extension_impl7callBar3bar3genyAA0H0C_AA3GenVySiSSGtF
func callBar(bar: Bar, gen: Gen<Int, String>) {
  // CHECK: [[BRIDGE:%.*]] = function_ref @$s42objc_bridged_using_protocol_extension_impl7FooablePAAs21_ObjectiveCBridgeableRzrlE09_bridgeToH1C01_H5CTypesADPQzyF
  // CHECK: apply [[BRIDGE]]<Gen<Int, String>>
  bar.bar(gen)
}
