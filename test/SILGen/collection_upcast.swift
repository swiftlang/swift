// RUN: %target-swift-frontend -emit-silgen -sdk %S/Inputs -I %S/Inputs -enable-source-import %s | %FileCheck %s

// FIXME: rdar://problem/19648117 Needs splitting objc parts out
// XFAIL: linux

import Foundation

// FIXME: Should go into the standard library.
public extension _ObjectiveCBridgeable {
  static func _unconditionallyBridgeFromObjectiveC(_ source: _ObjectiveCType?)
      -> Self {
    var result: Self?
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

// CHECK-LABEL: sil hidden @_TF17collection_upcast15testArrayUpcast{{.*}} :
// CHECK: bb0([[ARRAY:%[0-9]+]] : $Array<BridgedObjC>): 
func testArrayUpcast(_ array: [BridgedObjC]) {
  // CHECK: [[BORROWED_ARRAY:%.*]] = begin_borrow [[ARRAY]]
  // CHECK: [[ARRAY_COPY:%.*]] = copy_value [[BORROWED_ARRAY]]
  // CHECK: [[UPCAST_FN:%[0-9]+]] = function_ref @_TFs15_arrayForceCast{{.*}} : $@convention(thin) <τ_0_0, τ_0_1> (@owned Array<τ_0_0>) -> @owned Array<τ_0_1>
  // CHECK: [[RESULT:%.*]] = apply [[UPCAST_FN]]<BridgedObjC, AnyObject>([[ARRAY_COPY]]) : $@convention(thin) <τ_0_0, τ_0_1> (@owned Array<τ_0_0>) -> @owned Array<τ_0_1>
  //
  // => SEMANTIC SIL TODO: This is benign, but this end borrow should be after
  // the destroy_value
  //
  // CHECK: end_borrow [[BORROWED_ARRAY]] from [[ARRAY]]
  // CHECK: destroy_value [[RESULT]]
  // CHECK: destroy_value [[ARRAY]]
  let anyObjectArr: [AnyObject] = array
}
// CHECK: } // end sil function '_TF17collection_upcast15testArrayUpcast{{.*}}'

// CHECK-LABEL: sil hidden @_TF17collection_upcast22testArrayUpcastBridged
// CHECK: bb0([[ARRAY:%[0-9]+]] : $Array<BridgedSwift>):
func testArrayUpcastBridged(_ array: [BridgedSwift]) {
  // CHECK: [[BORROWED_ARRAY:%.*]] = begin_borrow [[ARRAY]]
  // CHECK: [[ARRAY_COPY:%.*]] = copy_value [[BORROWED_ARRAY]]
  // CHECK: [[BRIDGE_FN:%[0-9]+]] = function_ref @_TFs15_arrayForceCast{{.*}} : $@convention(thin) <τ_0_0, τ_0_1> (@owned Array<τ_0_0>) -> @owned Array<τ_0_1>
  // CHECK: [[RESULT:%.*]] = apply [[BRIDGE_FN]]<BridgedSwift, AnyObject>([[ARRAY_COPY]]) : $@convention(thin) <τ_0_0, τ_0_1> (@owned Array<τ_0_0>) -> @owned Array<τ_0_1>
  // CHECK: end_borrow [[BORROWED_ARRAY]] from [[ARRAY]]
  // CHECK: destroy_value [[RESULT]]
  // CHECK: destroy_value [[ARRAY]]
  let anyObjectArr = array as [AnyObject]
}
// CHECK: } // end sil function '_TF17collection_upcast22testArrayUpcastBridged{{.*}}'

// CHECK-LABEL: sil hidden @_TF17collection_upcast20testDictionaryUpcast
// CHECK: bb0([[DICT:%[0-9]+]] : $Dictionary<BridgedObjC, BridgedObjC>):
func testDictionaryUpcast(_ dict: Dictionary<BridgedObjC, BridgedObjC>) {
  // CHECK: [[BORROWED_DICT:%.*]] = begin_borrow [[DICT]]
  // CHECK: [[DICT_COPY:%.*]] = copy_value [[BORROWED_DICT]]
  // CHECK: [[UPCAST_FN:%[0-9]+]] = function_ref @_TFs17_dictionaryUpCast{{.*}} : $@convention(thin) <τ_0_0, τ_0_1, τ_0_2, τ_0_3 where τ_0_0 : Hashable, τ_0_2 : Hashable> (@owned Dictionary<τ_0_0, τ_0_1>) -> @owned Dictionary<τ_0_2, τ_0_3>
  // CHECK: [[RESULT:%.*]] = apply [[UPCAST_FN]]<BridgedObjC, BridgedObjC, NSObject, AnyObject>([[DICT_COPY]]) : $@convention(thin) <τ_0_0, τ_0_1, τ_0_2, τ_0_3 where τ_0_0 : Hashable, τ_0_2 : Hashable> (@owned Dictionary<τ_0_0, τ_0_1>) -> @owned Dictionary<τ_0_2, τ_0_3>
  // CHECK: end_borrow [[BORROWED_DICT]] from [[DICT]]
  // CHECK: destroy_value [[RESULT]]
  // CHECK: destroy_value [[DICT]]
  let anyObjectDict: Dictionary<NSObject, AnyObject> = dict
}

// CHECK-LABEL: sil hidden @_TF17collection_upcast27testDictionaryUpcastBridged
// CHECK: bb0([[DICT:%[0-9]+]] : $Dictionary<BridgedSwift, BridgedSwift>):
func testDictionaryUpcastBridged(_ dict: Dictionary<BridgedSwift, BridgedSwift>) {
  // CHECK: [[BORROWED_DICT:%.*]] = begin_borrow [[DICT]]
  // CHECK: [[DICT_COPY:%.*]] = copy_value [[BORROWED_DICT]]
  // CHECK: [[BRIDGE_FN:%[0-9]+]] = function_ref @_TFs17_dictionaryUpCast
  // CHECK: [[RESULT:%.*]] = apply [[BRIDGE_FN]]<BridgedSwift, BridgedSwift, NSObject, AnyObject>([[DICT_COPY]]) : $@convention(thin) <τ_0_0, τ_0_1, τ_0_2, τ_0_3 where τ_0_0 : Hashable, τ_0_2 : Hashable> (@owned Dictionary<τ_0_0, τ_0_1>) -> @owned Dictionary<τ_0_2, τ_0_3>
  // CHECK: end_borrow [[BORROWED_DICT]] from [[DICT]]
  // CHECK: destroy_value [[RESULT]]
  // CHECK: destroy_value [[DICT]]
  let anyObjectDict = dict as Dictionary<NSObject, AnyObject>
}

// CHECK-LABEL: sil hidden @_TF17collection_upcast13testSetUpcast
// CHECK: bb0([[SET:%[0-9]+]] : $Set<BridgedObjC>):
func testSetUpcast(_ dict: Set<BridgedObjC>) {
  // CHECK: [[BORROWED_SET:%.*]] = begin_borrow [[SET]]
  // CHECK: [[SET_COPY:%.*]] = copy_value [[BORROWED_SET]]
  // CHECK: [[BRIDGE_FN:%[0-9]+]] = function_ref @_TFs10_setUpCast{{.*}} : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_0 : Hashable, τ_0_1 : Hashable> (@owned Set<τ_0_0>) -> @owned Set<τ_0_1>
  // CHECK: [[RESULT:%.*]] = apply [[BRIDGE_FN]]<BridgedObjC, NSObject>([[SET_COPY]]) : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_0 : Hashable, τ_0_1 : Hashable> (@owned Set<τ_0_0>) -> @owned Set<τ_0_1>
  // CHECK: end_borrow [[BORROWED_SET]] from [[SET]]
  // CHECK: destroy_value [[RESULT]]
  // CHECK: destroy_value [[SET]]
  let anyObjectSet: Set<NSObject> = dict
}

// CHECK-LABEL: sil hidden @_TF17collection_upcast20testSetUpcastBridged
// CHECK: bb0([[SET:%.*]] : $Set<BridgedSwift>):
func testSetUpcastBridged(_ set: Set<BridgedSwift>) {
  // CHECK: [[BORROWED_SET:%.*]] = begin_borrow [[SET]]
  // CHECK: [[SET_COPY:%.*]] = copy_value [[BORROWED_SET]]
  // CHECK: [[BRIDGE_FN:%[0-9]+]] = function_ref @_TFs10_setUpCast
  // CHECK: [[RESULT:%.*]] = apply [[BRIDGE_FN]]<BridgedSwift, NSObject>([[SET_COPY]]) : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_0 : Hashable, τ_0_1 : Hashable> (@owned Set<τ_0_0>) -> @owned Set<τ_0_1>
  // CHECK: end_borrow [[BORROWED_SET]] from [[SET]]
  // CHECK: destroy_value [[RESULT]]
  // CHECK: destroy_value [[SET]]
  let anyObjectSet = set as Set<NSObject>
}
