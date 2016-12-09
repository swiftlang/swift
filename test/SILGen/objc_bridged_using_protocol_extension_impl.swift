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

// CHECK-LABEL: sil hidden @_TF42objc_bridged_using_protocol_extension_impl7callBarFT3barCS_3Bar3fooVS_3Foo_T_
func callBar(bar: Bar, foo: Foo) {
  // CHECK: [[BRIDGE:%.*]] = function_ref @_TFe42objc_bridged_using_protocol_extension_implRxs21_ObjectiveCBridgeablexS_7FooablerS1_19_bridgeToObjectiveCfT_wxPS0_15_ObjectiveCType
  // CHECK: apply [[BRIDGE]]<Foo>
  bar.bar(foo)
}

// CHECK-LABEL:sil hidden @_TF42objc_bridged_using_protocol_extension_impl7callBarFT3barCS_3Bar3genGVS_3GenSiSS__T_ 
func callBar(bar: Bar, gen: Gen<Int, String>) {
  // CHECK: [[BRIDGE:%.*]] = function_ref @_TFe42objc_bridged_using_protocol_extension_implRxs21_ObjectiveCBridgeablexS_7FooablerS1_19_bridgeToObjectiveCfT_wxPS0_15_ObjectiveCType
  // CHECK: apply [[BRIDGE]]<Gen<Int, String>>
  bar.bar(gen)
}
