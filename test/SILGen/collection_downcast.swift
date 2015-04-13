// RUN: %target-swift-frontend -emit-silgen -sdk %S/Inputs -I %S/Inputs -enable-source-import %s | FileCheck %s

// REQUIRES: objc_interop

import Foundation

class BridgedObjC : NSObject { }

func == (x: BridgedObjC, y: BridgedObjC) -> Bool { return true }

struct BridgedSwift : Hashable, _ObjectiveCBridgeable {
  static func _isBridgedToObjectiveC() -> Bool {
    return true
  }
  
  var hashValue: Int { return 0 }

  static func _getObjectiveCType() -> Any.Type {
    return BridgedObjC.self
  }
  
  func _bridgeToObjectiveC() -> BridgedObjC {
    return BridgedObjC()
  }

  static func _forceBridgeFromObjectiveC(
    x: BridgedObjC,
    inout result: BridgedSwift?
  ) {
  }
  static func _conditionallyBridgeFromObjectiveC(
    x: BridgedObjC,
    inout result: BridgedSwift?
  ) -> Bool {
    return true
  }
}

func == (x: BridgedSwift, y: BridgedSwift) -> Bool { return true }

// CHECK-LABEL: sil hidden @_TF19collection_downcast17testArrayDowncast
// CHECK: bb0([[ARRAY:%[0-9]+]] : $Array<AnyObject>):
func testArrayDowncast(array: [AnyObject]) -> [BridgedObjC] {
  // CHECK: [[DOWNCAST_FN:%[0-9]+]] = function_ref @_TFSs15_arrayForceCastU___FGSaQ__GSaQ0__ : $@convention(thin) <τ_0_0, τ_0_1> (@owned Array<τ_0_0>) -> @owned Array<τ_0_1>
  // CHECK: apply [[DOWNCAST_FN]]<AnyObject, BridgedObjC>([[ARRAY]]) : $@convention(thin) <τ_0_0, τ_0_1> (@owned Array<τ_0_0>) -> @owned Array<τ_0_1>
  return array as! [BridgedObjC]
}

// CHECK-LABEL: sil hidden @_TF19collection_downcast27testArrayDowncastFromObject
// CHECK: bb0([[OBJ:%[0-9]+]] : $AnyObject):
func testArrayDowncastFromObject(obj: AnyObject) -> [BridgedObjC] {
  // CHECK: unconditional_checked_cast_addr take_always AnyObject in [[OBJECT_ALLOC:%[0-9]+]]#1 : $*AnyObject to Array<BridgedObjC> in [[VALUE_ALLOC:%[0-9]+]]#1 : $*Array<BridgedObjC>
  return obj as! [BridgedObjC]
}

// CHECK-LABEL: sil hidden @_TF19collection_downcast28testArrayDowncastFromNSArray
// CHECK: bb0([[NSARRAY_OBJ:%[0-9]+]] : $NSArray):
func testArrayDowncastFromNSArray(obj: NSArray) -> [BridgedObjC] {
  // CHECK: unconditional_checked_cast_addr take_always NSArray in [[OBJECT_ALLOC:%[0-9]+]]#1 : $*NSArray to Array<BridgedObjC> in [[VALUE_ALLOC:%[0-9]+]]#1 : $*Array<BridgedObjC>
  return obj as! [BridgedObjC]
}

// CHECK-LABEL: sil hidden @_TF19collection_downcast28testArrayDowncastConditional
// CHECK: bb0([[ARRAY:%[0-9]+]] : $Array<AnyObject>):
func testArrayDowncastConditional(array: [AnyObject]) -> [BridgedObjC]? {
  // CHECK: [[DOWNCAST_FN:%[0-9]+]] = function_ref @_TFSs21_arrayConditionalCastU___FGSaQ__GSqGSaQ0___ : $@convention(thin) <τ_0_0, τ_0_1> (@owned Array<τ_0_0>) -> @owned Optional<Array<τ_0_1>>
  // CHECK-NEXT:  apply [[DOWNCAST_FN]]<AnyObject, BridgedObjC>([[ARRAY]]) : $@convention(thin) <τ_0_0, τ_0_1> (@owned Array<τ_0_0>) -> @owned Optional<Array<τ_0_1>>
  return array as? [BridgedObjC]
}

// CHECK-LABEL: sil hidden @_TF19collection_downcast12testArrayIsa
// CHECK: bb0([[ARRAY:%[0-9]+]] : $Array<AnyObject>)
func testArrayIsa(array: [AnyObject]) -> Bool {
  // CHECK: [[DOWNCAST_FN:%[0-9]+]] = function_ref @_TFSs21_arrayConditionalCastU___FGSaQ__GSqGSaQ0___ : $@convention(thin) <τ_0_0, τ_0_1> (@owned Array<τ_0_0>) -> @owned Optional<Array<τ_0_1>>
  // CHECK-NEXT: apply [[DOWNCAST_FN]]<AnyObject, BridgedObjC>([[ARRAY]]) : $@convention(thin) <τ_0_0, τ_0_1> (@owned Array<τ_0_0>) -> @owned Optional<Array<τ_0_1>>
  return array is [BridgedObjC] ? true : false
}

// CHECK-LABEL: sil hidden @_TF19collection_downcast24testArrayDowncastBridged
// CHECK: bb0([[ARRAY:%[0-9]+]] : $Array<AnyObject>):
func testArrayDowncastBridged(array: [AnyObject]) -> [BridgedSwift] {
  // CHECK: [[BRIDGE_FN:%[0-9]+]] = function_ref @_TFSs15_arrayForceCastU___FGSaQ__GSaQ0__ : $@convention(thin) <τ_0_0, τ_0_1> (@owned Array<τ_0_0>) -> @owned Array<τ_0_1>
  // CHECK-NEXT: apply [[BRIDGE_FN]]<AnyObject, BridgedSwift>([[ARRAY]]) : $@convention(thin) <τ_0_0, τ_0_1> (@owned Array<τ_0_0>) -> @owned Array<τ_0_1>
  return array as! [BridgedSwift]
}

// CHECK-LABEL: sil hidden @_TF19collection_downcast35testArrayDowncastBridgedConditional
// CHECK: bb0([[ARRAY:%[0-9]+]] : $Array<AnyObject>):
func testArrayDowncastBridgedConditional(array: [AnyObject]) -> [BridgedSwift]?{
  // CHECK: [[BRIDGE_FN:%[0-9]+]] = function_ref @_TFSs21_arrayConditionalCastU___FGSaQ__GSqGSaQ0___ : $@convention(thin) <τ_0_0, τ_0_1> (@owned Array<τ_0_0>) -> @owned Optional<Array<τ_0_1>>
  // CHECK-NEXT: apply [[BRIDGE_FN]]<AnyObject, BridgedSwift>([[ARRAY]]) : $@convention(thin) <τ_0_0, τ_0_1> (@owned Array<τ_0_0>) -> @owned Optional<Array<τ_0_1>>
  return array as? [BridgedSwift]
}

// CHECK-LABEL: sil hidden @_TF19collection_downcast19testArrayIsaBridged
// CHECK: bb0([[ARRAY:%[0-9]+]] : $Array<AnyObject>)
func testArrayIsaBridged(array: [AnyObject]) -> Bool {
  // CHECK: [[DOWNCAST_FN:%[0-9]+]] = function_ref @_TFSs21_arrayConditionalCastU___FGSaQ__GSqGSaQ0___ : $@convention(thin) <τ_0_0, τ_0_1> (@owned Array<τ_0_0>) -> @owned Optional<Array<τ_0_1>>
  // CHECK: apply [[DOWNCAST_FN]]<AnyObject, BridgedSwift>([[ARRAY]]) : $@convention(thin) <τ_0_0, τ_0_1> (@owned Array<τ_0_0>) -> @owned Optional<Array<τ_0_1>>
  return array is [BridgedSwift] ? true : false
}

// CHECK-LABEL: sil hidden @_TF19collection_downcast32testDictionaryDowncastFromObject
// CHECK: bb0([[OBJ:%[0-9]+]] : $AnyObject):
func testDictionaryDowncastFromObject(obj: AnyObject) 
       -> Dictionary<BridgedObjC, BridgedObjC> {
  // CHECK: unconditional_checked_cast_addr take_always AnyObject in [[OBJECT_ALLOC:%[0-9]+]]#1 : $*AnyObject to Dictionary<BridgedObjC, BridgedObjC> in [[VALUE_ALLOC:%[0-9]+]]#1 : $*Dictionary<BridgedObjC, BridgedObjC>
  return obj as! Dictionary<BridgedObjC, BridgedObjC>
}

// CHECK-LABEL: sil hidden @_TF19collection_downcast22testDictionaryDowncast
// CHECK: bb0([[DICT:%[0-9]+]] : $Dictionary<NSObject, AnyObject>)
func testDictionaryDowncast(dict: Dictionary<NSObject, AnyObject>) 
       -> Dictionary<BridgedObjC, BridgedObjC> {
  // CHECK: [[DOWNCAST_FN:%[0-9]+]] = function_ref @_TFSs19_dictionaryDownCastUSs8Hashable__S____FGVSs10DictionaryQ_Q0__GS0_Q1_Q2__ : $@convention(thin) <τ_0_0, τ_0_1, τ_0_2, τ_0_3 where τ_0_0 : Hashable, τ_0_2 : Hashable> (@owned Dictionary<τ_0_0, τ_0_1>) -> @owned Dictionary<τ_0_2, τ_0_3>
  // CHECK-NEXT: apply [[DOWNCAST_FN]]<NSObject, AnyObject, BridgedObjC, BridgedObjC>([[DICT]]) : $@convention(thin) <τ_0_0, τ_0_1, τ_0_2, τ_0_3 where τ_0_0 : Hashable, τ_0_2 : Hashable> (@owned Dictionary<τ_0_0, τ_0_1>) -> @owned Dictionary<τ_0_2, τ_0_3>
  return dict as! Dictionary<BridgedObjC, BridgedObjC>
}

// CHECK-LABEL: sil hidden @_TF19collection_downcast33testDictionaryDowncastConditional
// CHECK: bb0([[DICT:%[0-9]+]] : $Dictionary<NSObject, AnyObject>)
func testDictionaryDowncastConditional(dict: Dictionary<NSObject, AnyObject>) 
       -> Dictionary<BridgedObjC, BridgedObjC>? {
  // CHECK: [[DOWNCAST_FN:%[0-9]+]] = function_ref @_TFSs30_dictionaryDownCastConditionalUSs8Hashable__S____FGVSs10DictionaryQ_Q0__GSqGS0_Q1_Q2___ : $@convention(thin) <τ_0_0, τ_0_1, τ_0_2, τ_0_3 where τ_0_0 : Hashable, τ_0_2 : Hashable> (@owned Dictionary<τ_0_0, τ_0_1>) -> @owned Optional<Dictionary<τ_0_2, τ_0_3>>
  // CHECK-NEXT: apply [[DOWNCAST_FN]]<NSObject, AnyObject, BridgedObjC, BridgedObjC>([[DICT]]) : $@convention(thin) <τ_0_0, τ_0_1, τ_0_2, τ_0_3 where τ_0_0 : Hashable, τ_0_2 : Hashable> (@owned Dictionary<τ_0_0, τ_0_1>) -> @owned Optional<Dictionary<τ_0_2, τ_0_3>>
  return dict as? Dictionary<BridgedObjC, BridgedObjC>
}

// CHECK-LABEL: sil hidden @_TF19collection_downcast41testDictionaryDowncastBridgedVConditional
// CHECK: bb0([[DICT:%[0-9]+]] : $Dictionary<NSObject, AnyObject>)
func testDictionaryDowncastBridgedVConditional(dict: Dictionary<NSObject, AnyObject>) 
       -> Dictionary<BridgedObjC, BridgedSwift>? {
  // CHECK: [[BRIDGE_FN:%[0-9]+]] = function_ref @_TFSs42_dictionaryBridgeFromObjectiveCConditionalUSs8Hashable__S____FGVSs10DictionaryQ_Q0__GSqGS0_Q1_Q2___ : $@convention(thin) <τ_0_0, τ_0_1, τ_0_2, τ_0_3 where τ_0_0 : Hashable, τ_0_2 : Hashable> (@owned Dictionary<τ_0_0, τ_0_1>) -> @owned Optional<Dictionary<τ_0_2, τ_0_3>>
  // CHECK-NEXT: apply [[BRIDGE_FN]]<NSObject, AnyObject, BridgedObjC, BridgedSwift>([[DICT]]) : $@convention(thin) <τ_0_0, τ_0_1, τ_0_2, τ_0_3 where τ_0_0 : Hashable, τ_0_2 : Hashable> (@owned Dictionary<τ_0_0, τ_0_1>) -> @owned Optional<Dictionary<τ_0_2, τ_0_3>> // user: %6
  return dict as? Dictionary<BridgedObjC, BridgedSwift>
}

// CHECK-LABEL: sil hidden @_TF19collection_downcast41testDictionaryDowncastBridgedKConditional
// CHECK: bb0([[DICT:%[0-9]+]] : $Dictionary<NSObject, AnyObject>)
func testDictionaryDowncastBridgedKConditional(dict: Dictionary<NSObject, AnyObject>) 
       -> Dictionary<BridgedSwift, BridgedObjC>? {
  // CHECK: [[BRIDGE_FN:%[0-9]+]] = function_ref @_TFSs42_dictionaryBridgeFromObjectiveCConditionalUSs8Hashable__S____FGVSs10DictionaryQ_Q0__GSqGS0_Q1_Q2___ : $@convention(thin) <τ_0_0, τ_0_1, τ_0_2, τ_0_3 where τ_0_0 : Hashable, τ_0_2 : Hashable> (@owned Dictionary<τ_0_0, τ_0_1>) -> @owned Optional<Dictionary<τ_0_2, τ_0_3>>
  // CHECK-NEXT: apply [[BRIDGE_FN]]<NSObject, AnyObject, BridgedSwift, BridgedObjC>([[DICT]]) : $@convention(thin) <τ_0_0, τ_0_1, τ_0_2, τ_0_3 where τ_0_0 : Hashable, τ_0_2 : Hashable> (@owned Dictionary<τ_0_0, τ_0_1>) -> @owned Optional<Dictionary<τ_0_2, τ_0_3>>
  return dict as? Dictionary<BridgedSwift, BridgedObjC>
}

// CHECK-LABEL: sil hidden @_TF19collection_downcast31testDictionaryDowncastBridgedKV
// CHECK: bb0([[DICT:%[0-9]+]] : $Dictionary<NSObject, AnyObject>)
func testDictionaryDowncastBridgedKV(dict: Dictionary<NSObject, AnyObject>) 
       -> Dictionary<BridgedSwift, BridgedSwift> {
  // CHECK: [[BRIDGE_FN:%[0-9]+]] = function_ref @_TFSs31_dictionaryBridgeFromObjectiveCUSs8Hashable__S____FGVSs10DictionaryQ_Q0__GS0_Q1_Q2__ : $@convention(thin) <τ_0_0, τ_0_1, τ_0_2, τ_0_3 where τ_0_0 : Hashable, τ_0_2 : Hashable> (@owned Dictionary<τ_0_0, τ_0_1>) -> @owned Dictionary<τ_0_2, τ_0_3>
  // CHECK-NEXT: apply [[BRIDGE_FN]]<NSObject, AnyObject, BridgedSwift, BridgedSwift>([[DICT]]) : $@convention(thin) <τ_0_0, τ_0_1, τ_0_2, τ_0_3 where τ_0_0 : Hashable, τ_0_2 : Hashable> (@owned Dictionary<τ_0_0, τ_0_1>) -> @owned Dictionary<τ_0_2, τ_0_3>
  return dict as! Dictionary<BridgedSwift, BridgedSwift>
}

// CHECK-LABEL: sil hidden @_TF19collection_downcast42testDictionaryDowncastBridgedKVConditional
// CHECK: bb0([[DICT:%[0-9]+]] : $Dictionary<NSObject, AnyObject>)
func testDictionaryDowncastBridgedKVConditional(dict: Dictionary<NSObject, AnyObject>) 
       -> Dictionary<BridgedSwift, BridgedSwift>? {
  // CHECK: [[BRIDGE_FN:%[0-9]+]] = function_ref @_TFSs42_dictionaryBridgeFromObjectiveCConditionalUSs8Hashable__S____FGVSs10DictionaryQ_Q0__GSqGS0_Q1_Q2___ : $@convention(thin) <τ_0_0, τ_0_1, τ_0_2, τ_0_3 where τ_0_0 : Hashable, τ_0_2 : Hashable> (@owned Dictionary<τ_0_0, τ_0_1>) -> @owned Optional<Dictionary<τ_0_2, τ_0_3>>
  // CHECK-NEXT: apply [[BRIDGE_FN]]<NSObject, AnyObject, BridgedSwift, BridgedSwift>([[DICT]]) : $@convention(thin) <τ_0_0, τ_0_1, τ_0_2, τ_0_3 where τ_0_0 : Hashable, τ_0_2 : Hashable> (@owned Dictionary<τ_0_0, τ_0_1>) -> @owned Optional<Dictionary<τ_0_2, τ_0_3>>
  return dict as? Dictionary<BridgedSwift, BridgedSwift>
}

// CHECK-LABEL: sil hidden @_TF19collection_downcast25testSetDowncastFromObject
// CHECK: bb0([[OBJ:%[0-9]+]] : $AnyObject):
func testSetDowncastFromObject(obj: AnyObject) 
       -> Set<BridgedObjC> {
  // CHECK: unconditional_checked_cast_addr take_always AnyObject in [[OBJECT_ALLOC:%[0-9]+]]#1 : $*AnyObject to Set<BridgedObjC> in [[VALUE_ALLOC:%[0-9]+]]#1 : $*Set<BridgedObjC>
  return obj as! Set<BridgedObjC>
}

// CHECK-LABEL: sil hidden @_TF19collection_downcast15testSetDowncast
// CHECK: bb0([[SET:%[0-9]+]] : $Set<NSObject>)
func testSetDowncast(dict: Set<NSObject>) 
       -> Set<BridgedObjC> {
  // CHECK: [[DOWNCAST_FN:%[0-9]+]] = function_ref @_TFSs12_setDownCastUSs8Hashable_S___FGVSs3SetQ__GS0_Q0__ : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_0 : Hashable, τ_0_1 : Hashable> (@owned Set<τ_0_0>) -> @owned Set<τ_0_1>
  // CHECK-NEXT: apply [[DOWNCAST_FN]]<NSObject, BridgedObjC>([[SET]]) : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_0 : Hashable, τ_0_1 : Hashable> (@owned Set<τ_0_0>) -> @owned Set<τ_0_1>
  return dict as! Set<BridgedObjC>
}

// CHECK-LABEL: sil hidden @_TF19collection_downcast26testSetDowncastConditional
// CHECK: bb0([[SET:%[0-9]+]] : $Set<NSObject>)
func testSetDowncastConditional(dict: Set<NSObject>) 
       -> Set<BridgedObjC>? {
  // CHECK: [[DOWNCAST_FN:%[0-9]+]] = function_ref @_TFSs23_setDownCastConditionalUSs8Hashable_S___FGVSs3SetQ__GSqGS0_Q0___ : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_0 : Hashable, τ_0_1 : Hashable> (@owned Set<τ_0_0>) -> @owned Optional<Set<τ_0_1>>
  // CHECK-NEXT: apply [[DOWNCAST_FN]]<NSObject, BridgedObjC>([[SET]]) : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_0 : Hashable, τ_0_1 : Hashable> (@owned Set<τ_0_0>) -> @owned Optional<Set<τ_0_1>>
  return dict as? Set<BridgedObjC>
}

// CHECK-LABEL: sil hidden @_TF19collection_downcast22testSetDowncastBridged
// CHECK: bb0([[SET:%[0-9]+]] : $Set<NSObject>)
func testSetDowncastBridged(dict: Set<NSObject>) 
       -> Set<BridgedSwift> {
  // CHECK: [[DOWNCAST_FN:%[0-9]+]] = function_ref @_TFSs24_setBridgeFromObjectiveCUSs8Hashable_S___FGVSs3SetQ__GS0_Q0__ : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_0 : Hashable, τ_0_1 : Hashable> (@owned Set<τ_0_0>) -> @owned Set<τ_0_1>
  // CHECK-NEXT: apply [[DOWNCAST_FN]]<NSObject, BridgedSwift>([[SET]]) : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_0 : Hashable, τ_0_1 : Hashable> (@owned Set<τ_0_0>) -> @owned Set<τ_0_1>
  return dict as! Set<BridgedSwift>
}

// CHECK-LABEL: sil hidden @_TF19collection_downcast33testSetDowncastBridgedConditional
// CHECK: bb0([[SET:%[0-9]+]] : $Set<NSObject>)
func testSetDowncastBridgedConditional(dict: Set<NSObject>) 
       -> Set<BridgedSwift>? {
  return dict as? Set<BridgedSwift>
  // CHECK: [[DOWNCAST_FN:%[0-9]+]] = function_ref @_TFSs35_setBridgeFromObjectiveCConditionalUSs8Hashable_S___FGVSs3SetQ__GSqGS0_Q0___ : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_0 : Hashable, τ_0_1 : Hashable> (@owned Set<τ_0_0>) -> @owned Optional<Set<τ_0_1>>
  // CHECK: apply [[DOWNCAST_FN]]<NSObject, BridgedSwift>([[SET]]) : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_0 : Hashable, τ_0_1 : Hashable> (@owned Set<τ_0_0>) -> @owned Optional<Set<τ_0_1>>
}
