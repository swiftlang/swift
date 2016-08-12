// RUN: %target-swift-frontend -emit-silgen -sdk %S/Inputs -I %S/Inputs -enable-source-import %s | %FileCheck %s

// FIXME: rdar://problem/19648117 Needs splitting objc parts out
// XFAIL: linux

import Foundation

// FIXME: Should go into the standard library.
public extension _ObjectiveCBridgeable {
  static func _unconditionallyBridgeFromObjectiveC(_ source: _ObjectiveCType?)
      -> Self {
    var result: Self? = nil
    _forceBridgeFromObjectiveC(source!, result: &result)
    return result!
  }
}

class BridgedObjC : NSObject { }

func == (x: BridgedObjC, y: BridgedObjC) -> Bool { return true }

struct BridgedSwift : Hashable, _ObjectiveCBridgeable {
  var hashValue: Int { return 0 }

  func _bridgeToObjectiveC() -> BridgedObjC {
    return BridgedObjC()
  }

  static func _forceBridgeFromObjectiveC(
    _ x: BridgedObjC,
    result: inout BridgedSwift?
  ) {
  }
  static func _conditionallyBridgeFromObjectiveC(
    _ x: BridgedObjC,
    result: inout BridgedSwift?
  ) -> Bool {
    return true
  }
}

func == (x: BridgedSwift, y: BridgedSwift) -> Bool { return true }

// CHECK-LABEL: sil hidden @_TF17collection_upcast15testArrayUpcast
// CHECK: bb0([[ARRAY:%[0-9]+]] : $Array<BridgedObjC>): 
func testArrayUpcast(_ array: [BridgedObjC]) {
  // CHECK: [[UPCAST_FN:%[0-9]+]] = function_ref @_TFs15_arrayForceCast{{.*}} : $@convention(thin) <τ_0_0, τ_0_1> (@owned Array<τ_0_0>) -> @owned Array<τ_0_1>
  // CHECK-NEXT: apply [[UPCAST_FN]]<BridgedObjC, AnyObject>([[ARRAY]]) : $@convention(thin) <τ_0_0, τ_0_1> (@owned Array<τ_0_0>) -> @owned Array<τ_0_1>
  let anyObjectArr: [AnyObject] = array
}

// CHECK-LABEL: sil hidden @_TF17collection_upcast22testArrayUpcastBridged
// CHECK: bb0([[ARRAY:%[0-9]+]] : $Array<BridgedSwift>):
func testArrayUpcastBridged(_ array: [BridgedSwift]) {
  // CHECK: [[BRIDGE_FN:%[0-9]+]] = function_ref @_TFs15_arrayForceCast{{.*}} : $@convention(thin) <τ_0_0, τ_0_1> (@owned Array<τ_0_0>) -> @owned Array<τ_0_1>
  // CHECK-NEXT: apply [[BRIDGE_FN]]<BridgedSwift, AnyObject>([[ARRAY]]) : $@convention(thin) <τ_0_0, τ_0_1> (@owned Array<τ_0_0>) -> @owned Array<τ_0_1>
  let anyObjectArr = array as [AnyObject]
}

// CHECK-LABEL: sil hidden @_TF17collection_upcast20testDictionaryUpcast
// CHECK: bb0([[DICT:%[0-9]+]] : $Dictionary<BridgedObjC, BridgedObjC>):
func testDictionaryUpcast(_ dict: Dictionary<BridgedObjC, BridgedObjC>) {
  // CHECK: [[UPCAST_FN:%[0-9]+]] = function_ref @_TFs17_dictionaryUpCast{{.*}} : $@convention(thin) <τ_0_0, τ_0_1, τ_0_2, τ_0_3 where τ_0_0 : Hashable, τ_0_2 : Hashable> (@owned Dictionary<τ_0_0, τ_0_1>) -> @owned Dictionary<τ_0_2, τ_0_3>
  // CHECK-NEXT: apply [[UPCAST_FN]]<BridgedObjC, BridgedObjC, NSObject, AnyObject>([[DICT]]) : $@convention(thin) <τ_0_0, τ_0_1, τ_0_2, τ_0_3 where τ_0_0 : Hashable, τ_0_2 : Hashable> (@owned Dictionary<τ_0_0, τ_0_1>) -> @owned Dictionary<τ_0_2, τ_0_3>
  let anyObjectDict: Dictionary<NSObject, AnyObject> = dict
}

// CHECK-LABEL: sil hidden @_TF17collection_upcast27testDictionaryUpcastBridged
// CHECK: bb0([[DICT:%[0-9]+]] : $Dictionary<BridgedSwift, BridgedSwift>):
func testDictionaryUpcastBridged(_ dict: Dictionary<BridgedSwift, BridgedSwift>) {
  // CHECK: [[BRIDGE_FN:%[0-9]+]] = function_ref @_TFs17_dictionaryUpCast
  // CHECK-NEXT: apply [[BRIDGE_FN]]<BridgedSwift, BridgedSwift, NSObject, AnyObject>([[DICT]]) : $@convention(thin) <τ_0_0, τ_0_1, τ_0_2, τ_0_3 where τ_0_0 : Hashable, τ_0_2 : Hashable> (@owned Dictionary<τ_0_0, τ_0_1>) -> @owned Dictionary<τ_0_2, τ_0_3>
  let anyObjectDict = dict as Dictionary<NSObject, AnyObject>
}

// CHECK-LABEL: sil hidden @_TF17collection_upcast13testSetUpcast
// CHECK: bb0([[SET:%[0-9]+]] : $Set<BridgedObjC>):
func testSetUpcast(_ dict: Set<BridgedObjC>) {
  // CHECK: [[BRIDGE_FN:%[0-9]+]] = function_ref @_TFs10_setUpCast{{.*}} : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_0 : Hashable, τ_0_1 : Hashable> (@owned Set<τ_0_0>) -> @owned Set<τ_0_1>
  // CHECK-NEXT: apply [[BRIDGE_FN]]<BridgedObjC, NSObject>([[SET]]) : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_0 : Hashable, τ_0_1 : Hashable> (@owned Set<τ_0_0>) -> @owned Set<τ_0_1>
  let anyObjectSet: Set<NSObject> = dict
}

// CHECK-LABEL: sil hidden @_TF17collection_upcast20testSetUpcastBridged
// CHECK: bb0(%0 : $Set<BridgedSwift>):
func testSetUpcastBridged(_ set: Set<BridgedSwift>) {
  // CHECK: [[BRIDGE_FN:%[0-9]+]] = function_ref @_TFs10_setUpCast
  // CHECK: apply [[BRIDGE_FN]]<BridgedSwift, NSObject>([[SET]]) : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_0 : Hashable, τ_0_1 : Hashable> (@owned Set<τ_0_0>) -> @owned Set<τ_0_1>
  let anyObjectSet = set as Set<NSObject>
}
