
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -module-name collection_downcast -sdk %S/Inputs -I %S/Inputs -enable-source-import %s | %FileCheck %s

// REQUIRES: objc_interop

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
  func hash(into hasher: inout Hasher) {}

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

// CHECK-LABEL: sil hidden [ossa] @$s19collection_downcast17testArrayDowncast{{.*}}F
// CHECK: bb0([[ARRAY:%[0-9]+]] : @guaranteed $Array<AnyObject>):
func testArrayDowncast(_ array: [AnyObject]) -> [BridgedObjC] {
  // CHECK: [[ARRAY_COPY:%.*]] = copy_value [[ARRAY]]
  // CHECK: [[DOWNCAST_FN:%[0-9]+]] = function_ref @$ss15_arrayForceCast{{.*}}F
  // CHECK: apply [[DOWNCAST_FN]]<AnyObject, BridgedObjC>([[ARRAY_COPY]]) : $@convention(thin) <τ_0_0, τ_0_1> (@guaranteed Array<τ_0_0>) -> @owned Array<τ_0_1>
  return array as! [BridgedObjC]
}

// CHECK-LABEL: sil hidden [ossa] @$s19collection_downcast27testArrayDowncastFromObject{{.*}}F
// CHECK: bb0([[OBJ:%[0-9]+]] : @guaranteed $AnyObject):
func testArrayDowncastFromObject(_ obj: AnyObject) -> [BridgedObjC] {
  // CHECK: unconditional_checked_cast_addr AnyObject in [[OBJECT_ALLOC:%[0-9]+]] : $*AnyObject to Array<BridgedObjC> in [[VALUE_ALLOC:%[0-9]+]] : $*Array<BridgedObjC>
  return obj as! [BridgedObjC]
}

// CHECK-LABEL: sil hidden [ossa] @$s19collection_downcast28testArrayDowncastFromNSArray{{.*}}F
// CHECK: bb0([[NSARRAY_OBJ:%[0-9]+]] : @guaranteed $NSArray):
func testArrayDowncastFromNSArray(_ obj: NSArray) -> [BridgedObjC] {
  // CHECK: unconditional_checked_cast_addr NSArray in [[OBJECT_ALLOC:%[0-9]+]] : $*NSArray to Array<BridgedObjC> in [[VALUE_ALLOC:%[0-9]+]] : $*Array<BridgedObjC>
  return obj as! [BridgedObjC]
}

// CHECK-LABEL: sil hidden [ossa] @$s19collection_downcast28testArrayDowncastConditional{{.*}}F
// CHECK: bb0([[ARRAY:%[0-9]+]] : @guaranteed $Array<AnyObject>):
func testArrayDowncastConditional(_ array: [AnyObject]) -> [BridgedObjC]? {
  // CHECK: [[ARRAY_COPY:%.*]] = copy_value [[ARRAY]]
  // CHECK: [[DOWNCAST_FN:%[0-9]+]] = function_ref @$ss21_arrayConditionalCast{{.*}}F
  // CHECK: apply [[DOWNCAST_FN]]<AnyObject, BridgedObjC>([[ARRAY_COPY]]) : $@convention(thin) <τ_0_0, τ_0_1> (@guaranteed Array<τ_0_0>) -> @owned Optional<Array<τ_0_1>>
  return array as? [BridgedObjC]
}
// CHECK: } // end sil function '$s19collection_downcast28testArrayDowncastConditional{{.*}}F'

// CHECK-LABEL: sil hidden [ossa] @$s19collection_downcast12testArrayIsa{{.*}}F
// CHECK: bb0([[ARRAY:%[0-9]+]] : @guaranteed $Array<AnyObject>)
func testArrayIsa(_ array: [AnyObject]) -> Bool {
  // CHECK: [[ARRAY_COPY:%.*]] = copy_value [[ARRAY]]
  // CHECK: [[DOWNCAST_FN:%[0-9]+]] = function_ref @$ss21_arrayConditionalCast{{.*}}F
  // CHECK: apply [[DOWNCAST_FN]]<AnyObject, BridgedObjC>([[ARRAY_COPY]]) : $@convention(thin) <τ_0_0, τ_0_1> (@guaranteed Array<τ_0_0>) -> @owned Optional<Array<τ_0_1>>
  // CHECK-NOT: destroy_value [[ARRAY]]
  return array is [BridgedObjC] ? true : false
}

// CHECK-LABEL: sil hidden [ossa] @$s19collection_downcast24testArrayDowncastBridged{{.*}}F
// CHECK: bb0([[ARRAY:%[0-9]+]] : @guaranteed $Array<AnyObject>):
func testArrayDowncastBridged(_ array: [AnyObject]) -> [BridgedSwift] {
  // CHECK: [[ARRAY_COPY:%.*]] = copy_value [[ARRAY]]
  // CHECK: [[BRIDGE_FN:%[0-9]+]] = function_ref @$ss15_arrayForceCast{{.*}}F
  // CHECK: apply [[BRIDGE_FN]]<AnyObject, BridgedSwift>([[ARRAY_COPY]]) : $@convention(thin) <τ_0_0, τ_0_1> (@guaranteed Array<τ_0_0>) -> @owned Array<τ_0_1>
  // CHECK-NOT: destroy_value [[ARRAY]]
  return array as! [BridgedSwift]
}

// CHECK-LABEL: sil hidden [ossa] @$s19collection_downcast35testArrayDowncastBridgedConditional{{.*}}F
// CHECK: bb0([[ARRAY:%[0-9]+]] : @guaranteed $Array<AnyObject>):
func testArrayDowncastBridgedConditional(_ array: [AnyObject]) -> [BridgedSwift]?{
  // CHECK: [[ARRAY_COPY:%.*]] = copy_value [[ARRAY]]
  // CHECK: [[BRIDGE_FN:%[0-9]+]] = function_ref @$ss21_arrayConditionalCast{{.*}}F
  // CHECK: apply [[BRIDGE_FN]]<AnyObject, BridgedSwift>([[ARRAY_COPY]]) : $@convention(thin) <τ_0_0, τ_0_1> (@guaranteed Array<τ_0_0>) -> @owned Optional<Array<τ_0_1>>
  // CHECK-NOT: destroy_value [[ARRAY]]
  return array as? [BridgedSwift]
}

// CHECK-LABEL: sil hidden [ossa] @$s19collection_downcast19testArrayIsaBridged{{.*}}F
// CHECK: bb0([[ARRAY:%[0-9]+]] : @guaranteed $Array<AnyObject>)
func testArrayIsaBridged(_ array: [AnyObject]) -> Bool {
  // CHECK: [[ARRAY_COPY:%.*]] = copy_value [[ARRAY]]
  // CHECK: [[DOWNCAST_FN:%[0-9]+]] = function_ref @$ss21_arrayConditionalCast{{.*}}F
  // CHECK: apply [[DOWNCAST_FN]]<AnyObject, BridgedSwift>([[ARRAY_COPY]]) : $@convention(thin) <τ_0_0, τ_0_1> (@guaranteed Array<τ_0_0>) -> @owned Optional<Array<τ_0_1>>
  // CHECK-NOT: destroy_value [[ARRAY]]
  return array is [BridgedSwift] ? true : false
}

// CHECK-LABEL: sil hidden [ossa] @$s19collection_downcast32testDictionaryDowncastFromObject{{.*}}F
// CHECK: bb0([[OBJ:%[0-9]+]] : @guaranteed $AnyObject):
func testDictionaryDowncastFromObject(_ obj: AnyObject) 
       -> Dictionary<BridgedObjC, BridgedObjC> {
  // CHECK: unconditional_checked_cast_addr AnyObject in [[OBJECT_ALLOC:%[0-9]+]] : $*AnyObject to Dictionary<BridgedObjC, BridgedObjC> in [[VALUE_ALLOC:%[0-9]+]] : $*Dictionary<BridgedObjC, BridgedObjC>
  return obj as! Dictionary<BridgedObjC, BridgedObjC>
}

// CHECK-LABEL: sil hidden [ossa] @$s19collection_downcast22testDictionaryDowncast{{.*}}F
// CHECK: bb0([[DICT:%[0-9]+]] : @guaranteed $Dictionary<NSObject, AnyObject>)
func testDictionaryDowncast(_ dict: Dictionary<NSObject, AnyObject>) 
       -> Dictionary<BridgedObjC, BridgedObjC> {
  // CHECK: [[DICT_COPY:%.*]] = copy_value [[DICT]]
  // CHECK: [[DOWNCAST_FN:%[0-9]+]] = function_ref @$ss19_dictionaryDownCast{{.*}}F
  // CHECK: apply [[DOWNCAST_FN]]<NSObject, AnyObject, BridgedObjC, BridgedObjC>([[DICT_COPY]]) : $@convention(thin) <τ_0_0, τ_0_1, τ_0_2, τ_0_3 where τ_0_0 : Hashable, τ_0_2 : Hashable> (@guaranteed Dictionary<τ_0_0, τ_0_1>) -> @owned Dictionary<τ_0_2, τ_0_3>
  // CHECK-NOT: destroy_value [[DICT]]
  return dict as! Dictionary<BridgedObjC, BridgedObjC>
}

// CHECK-LABEL: sil hidden [ossa] @$s19collection_downcast33testDictionaryDowncastConditional{{.*}}F
// CHECK: bb0([[DICT:%[0-9]+]] : @guaranteed $Dictionary<NSObject, AnyObject>)
func testDictionaryDowncastConditional(_ dict: Dictionary<NSObject, AnyObject>) 
-> Dictionary<BridgedObjC, BridgedObjC>? {
  // CHECK: [[DICT_COPY:%.*]] = copy_value [[DICT]]
  // CHECK: [[DOWNCAST_FN:%[0-9]+]] = function_ref @$ss30_dictionaryDownCastConditional{{.*}}F
  // CHECK: apply [[DOWNCAST_FN]]<NSObject, AnyObject, BridgedObjC, BridgedObjC>([[DICT_COPY]]) : $@convention(thin) <τ_0_0, τ_0_1, τ_0_2, τ_0_3 where τ_0_0 : Hashable, τ_0_2 : Hashable> (@guaranteed Dictionary<τ_0_0, τ_0_1>) -> @owned Optional<Dictionary<τ_0_2, τ_0_3>>
  // CHECK-NOT: destroy_value [[DICT]]
  return dict as? Dictionary<BridgedObjC, BridgedObjC>
}

// CHECK-LABEL: sil hidden [ossa] @$s19collection_downcast41testDictionaryDowncastBridgedVConditional{{.*}}F
// CHECK: bb0([[DICT:%[0-9]+]] : @guaranteed $Dictionary<NSObject, AnyObject>)
func testDictionaryDowncastBridgedVConditional(_ dict: Dictionary<NSObject, AnyObject>) 
       -> Dictionary<BridgedObjC, BridgedSwift>? {
  // CHECK: [[DICT_COPY:%.*]] = copy_value [[DICT]]
  // CHECK: [[BRIDGE_FN:%[0-9]+]] = function_ref @$ss30_dictionaryDownCastConditional{{.*}}F
  // CHECK: apply [[BRIDGE_FN]]<NSObject, AnyObject, BridgedObjC, BridgedSwift>([[DICT_COPY]]) : $@convention(thin) <τ_0_0, τ_0_1, τ_0_2, τ_0_3 where τ_0_0 : Hashable, τ_0_2 : Hashable> (@guaranteed Dictionary<τ_0_0, τ_0_1>) -> @owned Optional<Dictionary<τ_0_2, τ_0_3>>{{.*}}
  // CHECK-NOT: destroy_value [[DICT]]
  return dict as? Dictionary<BridgedObjC, BridgedSwift>
}

// CHECK-LABEL: sil hidden [ossa] @$s19collection_downcast41testDictionaryDowncastBridgedKConditional{{.*}}F
// CHECK: bb0([[DICT:%[0-9]+]] : @guaranteed $Dictionary<NSObject, AnyObject>)
func testDictionaryDowncastBridgedKConditional(_ dict: Dictionary<NSObject, AnyObject>) 
-> Dictionary<BridgedSwift, BridgedObjC>? {
  // CHECK: [[DICT_COPY:%.*]] = copy_value [[DICT]]
  // CHECK: [[BRIDGE_FN:%[0-9]+]] = function_ref @$ss30_dictionaryDownCastConditional{{.*}}F
  // CHECK: apply [[BRIDGE_FN]]<NSObject, AnyObject, BridgedSwift, BridgedObjC>([[DICT_COPY]]) : $@convention(thin) <τ_0_0, τ_0_1, τ_0_2, τ_0_3 where τ_0_0 : Hashable, τ_0_2 : Hashable> (@guaranteed Dictionary<τ_0_0, τ_0_1>) -> @owned Optional<Dictionary<τ_0_2, τ_0_3>>
  // CHECK-NOT: destroy_value [[DICT]]
  return dict as? Dictionary<BridgedSwift, BridgedObjC>
}

// CHECK-LABEL: sil hidden [ossa] @$s19collection_downcast31testDictionaryDowncastBridgedKV{{.*}}F
// CHECK: bb0([[DICT:%[0-9]+]] : @guaranteed $Dictionary<NSObject, AnyObject>)
func testDictionaryDowncastBridgedKV(_ dict: Dictionary<NSObject, AnyObject>) 
-> Dictionary<BridgedSwift, BridgedSwift> {
  // CHECK: [[DICT_COPY:%.*]] = copy_value [[DICT]]
  // CHECK: [[BRIDGE_FN:%[0-9]+]] = function_ref @$ss19_dictionaryDownCast{{.*}}F
  // CHECK: apply [[BRIDGE_FN]]<NSObject, AnyObject, BridgedSwift, BridgedSwift>([[DICT_COPY]]) : $@convention(thin) <τ_0_0, τ_0_1, τ_0_2, τ_0_3 where τ_0_0 : Hashable, τ_0_2 : Hashable> (@guaranteed Dictionary<τ_0_0, τ_0_1>) -> @owned Dictionary<τ_0_2, τ_0_3>
  // CHECK-NOT: destroy_value [[DICT]]
  return dict as! Dictionary<BridgedSwift, BridgedSwift>
}

// CHECK-LABEL: sil hidden [ossa] @$s19collection_downcast42testDictionaryDowncastBridgedKVConditional{{.*}}F
// CHECK: bb0([[DICT:%[0-9]+]] : @guaranteed $Dictionary<NSObject, AnyObject>)
func testDictionaryDowncastBridgedKVConditional(_ dict: Dictionary<NSObject, AnyObject>) 
       -> Dictionary<BridgedSwift, BridgedSwift>? {
  // CHECK: [[DICT_COPY:%.*]] = copy_value [[DICT]]
  // CHECK: [[BRIDGE_FN:%[0-9]+]] = function_ref @$ss30_dictionaryDownCastConditional{{.*}}F
  // CHECK: apply [[BRIDGE_FN]]<NSObject, AnyObject, BridgedSwift, BridgedSwift>([[DICT_COPY]]) : $@convention(thin) <τ_0_0, τ_0_1, τ_0_2, τ_0_3 where τ_0_0 : Hashable, τ_0_2 : Hashable> (@guaranteed Dictionary<τ_0_0, τ_0_1>) -> @owned Optional<Dictionary<τ_0_2, τ_0_3>>
  // CHECK-NOT: destroy_value [[DICT]]
  return dict as? Dictionary<BridgedSwift, BridgedSwift>
}

// CHECK-LABEL: sil hidden [ossa] @$s19collection_downcast25testSetDowncastFromObject{{.*}}F
// CHECK: bb0([[OBJ:%[0-9]+]] : @guaranteed $AnyObject):
func testSetDowncastFromObject(_ obj: AnyObject) 
       -> Set<BridgedObjC> {
  // CHECK: unconditional_checked_cast_addr AnyObject in [[OBJECT_ALLOC:%[0-9]+]] : $*AnyObject to Set<BridgedObjC> in [[VALUE_ALLOC:%[0-9]+]] : $*Set<BridgedObjC>
  return obj as! Set<BridgedObjC>
}

// CHECK-LABEL: sil hidden [ossa] @$s19collection_downcast15testSetDowncast{{.*}}F
// CHECK: bb0([[SET:%[0-9]+]] : @guaranteed $Set<NSObject>)
func testSetDowncast(_ dict: Set<NSObject>) 
       -> Set<BridgedObjC> {
  // CHECK: [[SET_COPY:%.*]] = copy_value [[SET]]
  // CHECK: [[DOWNCAST_FN:%[0-9]+]] = function_ref @$ss12_setDownCast{{.*}}F
  // CHECK: apply [[DOWNCAST_FN]]<NSObject, BridgedObjC>([[SET_COPY]]) : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_0 : Hashable, τ_0_1 : Hashable> (@guaranteed Set<τ_0_0>) -> @owned Set<τ_0_1>
  // CHECK-NOT: destroy_value [[SET]]
  return dict as! Set<BridgedObjC>
}

// CHECK-LABEL: sil hidden [ossa] @$s19collection_downcast26testSetDowncastConditional{{.*}}F
// CHECK: bb0([[SET:%[0-9]+]] : @guaranteed $Set<NSObject>)
func testSetDowncastConditional(_ dict: Set<NSObject>) 
       -> Set<BridgedObjC>? {
  // CHECK: [[SET_COPY:%.*]] = copy_value [[SET]]
  // CHECK: [[DOWNCAST_FN:%[0-9]+]] = function_ref @$ss23_setDownCastConditional{{.*}}F
  // CHECK: apply [[DOWNCAST_FN]]<NSObject, BridgedObjC>([[SET_COPY]]) : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_0 : Hashable, τ_0_1 : Hashable> (@guaranteed Set<τ_0_0>) -> @owned Optional<Set<τ_0_1>>
  // CHECK-NOT: destroy_value [[SET]]
  return dict as? Set<BridgedObjC>
}

// CHECK-LABEL: sil hidden [ossa] @$s19collection_downcast22testSetDowncastBridged{{.*}}F
// CHECK: bb0([[SET:%[0-9]+]] : @guaranteed $Set<NSObject>)
func testSetDowncastBridged(_ dict: Set<NSObject>) 
       -> Set<BridgedSwift> {
  // CHECK: [[SET_COPY:%.*]] = copy_value [[SET]]
  // CHECK: [[DOWNCAST_FN:%[0-9]+]] = function_ref @$ss12_setDownCast{{.*}}F
  // CHECK: apply [[DOWNCAST_FN]]<NSObject, BridgedSwift>([[SET_COPY]]) : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_0 : Hashable, τ_0_1 : Hashable> (@guaranteed Set<τ_0_0>) -> @owned Set<τ_0_1>
  // CHECK-NOT: destroy_value [[SET]]
  return dict as! Set<BridgedSwift>
}

// CHECK-LABEL: sil hidden [ossa] @$s19collection_downcast33testSetDowncastBridgedConditional{{.*}}F
// CHECK: bb0([[SET:%[0-9]+]] : @guaranteed $Set<NSObject>)
func testSetDowncastBridgedConditional(_ dict: Set<NSObject>) 
       -> Set<BridgedSwift>? {
  // CHECK: [[SET_COPY:%.*]] = copy_value [[SET]]
  // CHECK: [[DOWNCAST_FN:%[0-9]+]] = function_ref @$ss23_setDownCastConditional{{.*}}F
  // CHECK: apply [[DOWNCAST_FN]]<NSObject, BridgedSwift>([[SET_COPY]]) : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_0 : Hashable, τ_0_1 : Hashable> (@guaranteed Set<τ_0_0>) -> @owned Optional<Set<τ_0_1>>
  // CHECK-NOT: destroy_value [[SET]]
  return dict as? Set<BridgedSwift>
}

func promote<T>(_ x: T) -> T? { nil }

// CHECK-LABEL: sil hidden [ossa] @$s19collection_downcast36testCollectionCompatibilityCoercionsyySaySiG_SayypGSgShySSGSDySSSiGSitF
func testCollectionCompatibilityCoercions(_ arr: [Int], _ optArr: [Any]?, _ set: Set<String>, _ dict: [String: Int], _ i: Int) {
  // Make sure we generate reasonable code for all of the below examples.

  // CHECK: [[CAST_FN:%.+]] = function_ref @$ss15_arrayForceCastySayq_GSayxGr0_lF : $@convention(thin) <τ_0_0, τ_0_1> (@guaranteed Array<τ_0_0>) -> @owned Array<τ_0_1>
  // CHECK: apply [[CAST_FN]]<Any, NSObject>({{%.+}}) : $@convention(thin) <τ_0_0, τ_0_1> (@guaranteed Array<τ_0_0>) -> @owned Array<τ_0_1>
  _ = (optArr ?? []) as [NSObject]

  // CHECK: [[CAST_FN:%.+]] = function_ref @$ss15_arrayForceCastySayq_GSayxGr0_lF : $@convention(thin) <τ_0_0, τ_0_1> (@guaranteed Array<τ_0_0>) -> @owned Array<τ_0_1>
  // CHECK: [[RESULT:%.+]] = apply [[CAST_FN]]<Any, Optional<String>>({{%.+}}) : $@convention(thin) <τ_0_0, τ_0_1> (@guaranteed Array<τ_0_0>) -> @owned Array<τ_0_1>
  // CHECK: enum $Optional<Array<Optional<String>>>, #Optional.some!enumelt, [[RESULT]] : $Array<Optional<String>>
  _ = (optArr ?? []) as [String?]?

  // CHECK: switch_enum {{%.+}} : $Optional<Optional<Optional<Array<Int>>>>
  // CHECK: switch_enum {{%.+}} : $Optional<Optional<Array<Int>>>
  // CHECK: switch_enum {{%.+}} : $Optional<Array<Int>>
  // CHECK: bb{{[0-9]+}}([[ARR:%.+]] : @owned $Array<Int>)
  // CHECK: [[CAST_FN:%.+]] = function_ref @$ss15_arrayForceCastySayq_GSayxGr0_lF : $@convention(thin) <τ_0_0, τ_0_1> (@guaranteed Array<τ_0_0>) -> @owned Array<τ_0_1>
  // CHECK: apply [[CAST_FN]]<Int, Optional<Float>>([[ARR]]) : $@convention(thin) <τ_0_0, τ_0_1> (@guaranteed Array<τ_0_0>) -> @owned Array<τ_0_1>
  _ = promote(promote(promote(arr))) as [Float?]

  // CHECK: [[CAST_FN:%.+]] = function_ref @$ss10_setUpCastyShyq_GShyxGSHRzSHR_r0_lF : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_0 : Hashable, τ_0_1 : Hashable> (@guaranteed Set<τ_0_0>) -> @owned Set<τ_0_1>
  // CHECK: apply [[CAST_FN]]<String, Int>({{%.+}}) : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_0 : Hashable, τ_0_1 : Hashable> (@guaranteed Set<τ_0_0>) -> @owned Set<τ_0_1>
  _ = (set ?? []) as Set<Int>

  // CHECK: [[CAST_FN:%.+]] = function_ref @$ss10_setUpCastyShyq_GShyxGSHRzSHR_r0_lF : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_0 : Hashable, τ_0_1 : Hashable> (@guaranteed Set<τ_0_0>) -> @owned Set<τ_0_1>
  // CHECK: [[RESULT:%.+]] = apply [[CAST_FN]]<String, Optional<Float>>({{%.+}}) : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_0 : Hashable, τ_0_1 : Hashable> (@guaranteed Set<τ_0_0>) -> @owned Set<τ_0_1>
  // CHECK: [[OPT_RESULT:%.+]] = enum $Optional<Set<Optional<Float>>>, #Optional.some!enumelt, [[RESULT]] : $Set<Optional<Float>>
  // CHECK: enum $Optional<Optional<Set<Optional<Float>>>>, #Optional.some!enumelt, [[OPT_RESULT]] : $Optional<Set<Optional<Float>>>
  _ = (set ?? []) as Set<Float?>??

  // CHECK: switch_enum {{%.+}} : $Optional<Optional<Optional<Set<String>>>>
  // CHECK: switch_enum {{%.+}} : $Optional<Optional<Set<String>>>
  // CHECK: switch_enum {{%.+}} : $Optional<Set<String>>
  // CHECK: bb{{[0-9]+}}([[SET:%.+]] : @owned $Set<String>)
  // CHECK: [[CAST_FN:%.+]] = function_ref @$ss10_setUpCastyShyq_GShyxGSHRzSHR_r0_lF : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_0 : Hashable, τ_0_1 : Hashable> (@guaranteed Set<τ_0_0>) -> @owned Set<τ_0_1>
  // CHECK: apply [[CAST_FN]]<String, Int>([[SET]]) : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_0 : Hashable, τ_0_1 : Hashable> (@guaranteed Set<τ_0_0>) -> @owned Set<τ_0_1>
  _ = promote(promote(promote(set))) as Set<Int>

  // CHECK: [[CAST_FN:%.+]] = function_ref @$ss17_dictionaryUpCastySDyq0_q1_GSDyxq_GSHRzSHR0_r2_lF : $@convention(thin) <τ_0_0, τ_0_1, τ_0_2, τ_0_3 where τ_0_0 : Hashable, τ_0_2 : Hashable> (@guaranteed Dictionary<τ_0_0, τ_0_1>) -> @owned Dictionary<τ_0_2, τ_0_3>
  // CHECK: apply [[CAST_FN]]<String, Int, Float, String>({{%.+}}) : $@convention(thin) <τ_0_0, τ_0_1, τ_0_2, τ_0_3 where τ_0_0 : Hashable, τ_0_2 : Hashable> (@guaranteed Dictionary<τ_0_0, τ_0_1>) -> @owned Dictionary<τ_0_2, τ_0_3>
  _ = (dict ?? [:]) as [Float: String]

  // CHECK: [[CAST_FN:%.+]] = function_ref @$ss17_dictionaryUpCastySDyq0_q1_GSDyxq_GSHRzSHR0_r2_lF : $@convention(thin) <τ_0_0, τ_0_1, τ_0_2, τ_0_3 where τ_0_0 : Hashable, τ_0_2 : Hashable> (@guaranteed Dictionary<τ_0_0, τ_0_1>) -> @owned Dictionary<τ_0_2, τ_0_3>
  // CHECK: [[RESULT:%.+]] = apply [[CAST_FN]]<String, Int, Double, Optional<Int>>({{%.+}}) : $@convention(thin) <τ_0_0, τ_0_1, τ_0_2, τ_0_3 where τ_0_0 : Hashable, τ_0_2 : Hashable> (@guaranteed Dictionary<τ_0_0, τ_0_1>) -> @owned Dictionary<τ_0_2, τ_0_3>
  // CHECK: enum $Optional<Dictionary<Double, Optional<Int>>>, #Optional.some!enumelt, [[RESULT]] : $Dictionary<Double, Optional<Int>>
  _ = (dict ?? [:]) as [Double: Int?]?

  // CHECK: switch_enum {{%.+}} : $Optional<Optional<Optional<Dictionary<String, Int>>>>
  // CHECK: switch_enum {{%.+}} : $Optional<Optional<Dictionary<String, Int>>>
  // CHECK: switch_enum {{%.+}} : $Optional<Dictionary<String, Int>>
  // CHECK: bb{{[0-9]+}}([[DICT:%.+]] : @owned $Dictionary<String, Int>)
  // CHECK: [[CAST_FN:%.+]] = function_ref @$ss17_dictionaryUpCastySDyq0_q1_GSDyxq_GSHRzSHR0_r2_lF : $@convention(thin) <τ_0_0, τ_0_1, τ_0_2, τ_0_3 where τ_0_0 : Hashable, τ_0_2 : Hashable> (@guaranteed Dictionary<τ_0_0, τ_0_1>) -> @owned Dictionary<τ_0_2, τ_0_3>
  // CHECK: apply [[CAST_FN]]<String, Int, Int, String>([[DICT]]) : $@convention(thin) <τ_0_0, τ_0_1, τ_0_2, τ_0_3 where τ_0_0 : Hashable, τ_0_2 : Hashable> (@guaranteed Dictionary<τ_0_0, τ_0_1>) -> @owned Dictionary<τ_0_2, τ_0_3>
  _ = promote(promote(promote(dict))) as [Int: String]

  typealias Magic<T> = T

  // These are currently not peepholed.
  // CHECK: [[CAST_FN:%.+]] = function_ref @$ss15_arrayForceCastySayq_GSayxGr0_lF : $@convention(thin) <τ_0_0, τ_0_1> (@guaranteed Array<τ_0_0>) -> @owned Array<τ_0_1>
  // CHECK: apply [[CAST_FN]]<Int, String>
  [i].self as Magic as [String]

  // CHECK: [[CAST_FN:%.+]] = function_ref @$ss15_arrayForceCastySayq_GSayxGr0_lF : $@convention(thin) <τ_0_0, τ_0_1> (@guaranteed Array<τ_0_0>) -> @owned Array<τ_0_1>
  // CHECK: apply [[CAST_FN]]<Int, String>
  (try [i]) as Magic as [String]
}
