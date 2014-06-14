// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -emit-silgen -module-cache-path %t/clang-module-cache -target x86_64-apple-darwin13 -sdk %S/Inputs -I %S/Inputs -enable-source-import %s | FileCheck %s

import Foundation

class BridgedObjC : NSObject { }

func == (x: BridgedObjC, y: BridgedObjC) -> Bool { return true }

struct BridgedSwift : Hashable, _BridgedToObjectiveC {
  var hashValue: Int { return 0 }

  static func getObjectiveCType() -> Any.Type {
    return BridgedObjC.self
  }
  
  func bridgeToObjectiveC() -> BridgedObjC {
    return BridgedObjC()
  }

  static func bridgeFromObjectiveC(x: BridgedObjC) -> BridgedSwift? {
    return nil
  }
}

func == (x: BridgedSwift, y: BridgedSwift) -> Bool { return true }

// CHECK-LABEL: sil @_TF17collection_upcast15testArrayUpcast
// CHECK-NEXT: bb0([[ARRAY:%[0-9]+]] : $Array<BridgedObjC>): 
func testArrayUpcast(array: BridgedObjC[]) {
  // CHECK: [[UPCAST_FN:%[0-9]+]] = function_ref @_TFSs12_arrayUpCastU___FGSaQ__GSaQ0__ : $@thin <τ_0_0, τ_0_1> (@owned Array<τ_0_0>) -> @owned Array<τ_0_1>
  // CHECK-NEXT: apply [[UPCAST_FN]]<BridgedObjC, AnyObject>([[ARRAY]]) : $@thin <τ_0_0, τ_0_1> (@owned Array<τ_0_0>) -> @owned Array<τ_0_1>
  let anyObjectArr: AnyObject[] = array
}

// CHECK-LABEL: sil @_TF17collection_upcast22testArrayUpcastBridged
// CHECK-NEXT: bb0([[ARRAY:%[0-9]+]] : $Array<BridgedSwift>):
func testArrayUpcastBridged(array: BridgedSwift[]) {
  // CHECK: [[BRIDGE_FN:%[0-9]+]] = function_ref @_TFSs24_arrayBridgeToObjectiveCU___FGSaQ__GSaQ0__ : $@thin <τ_0_0, τ_0_1> (@owned Array<τ_0_0>) -> @owned Array<τ_0_1>
  // CHECK-NEXT: apply [[BRIDGE_FN]]<BridgedSwift, AnyObject>([[ARRAY]]) : $@thin <τ_0_0, τ_0_1> (@owned Array<τ_0_0>) -> @owned Array<τ_0_1>
  let anyObjectArr: AnyObject[] = array
}

// CHECK-LABEL: sil @_TF17collection_upcast20testDictionaryUpcast
// CHECK-NEXT: bb0([[DICT:%[0-9]+]] : $Dictionary<BridgedObjC, BridgedObjC>):
func testDictionaryUpcast(dict: Dictionary<BridgedObjC, BridgedObjC>) {
  // CHECK: [[UPCAST_FN:%[0-9]+]] = function_ref @_TFSs17_dictionaryUpCastUSs8Hashable__S____FGVSs10DictionaryQ_Q0__GS0_Q1_Q2__ : $@thin <τ_0_0, τ_0_1, τ_0_2, τ_0_3 where τ_0_0 : Hashable, τ_0_2 : Hashable> (@owned Dictionary<τ_0_0, τ_0_1>) -> @owned Dictionary<τ_0_2, τ_0_3>
  // CHECK-NEXT: apply [[UPCAST_FN]]<BridgedObjC, BridgedObjC, NSObject, AnyObject>([[DICT]]) : $@thin <τ_0_0, τ_0_1, τ_0_2, τ_0_3 where τ_0_0 : Hashable, τ_0_2 : Hashable> (@owned Dictionary<τ_0_0, τ_0_1>) -> @owned Dictionary<τ_0_2, τ_0_3>
  let anyObjectDict: Dictionary<NSObject, AnyObject> = dict
}

// CHECK-LABEL: sil @_TF17collection_upcast27testDictionaryUpcastBridged
// CHECK-NEXT: bb0([[DICT:%[0-9]+]] : $Dictionary<BridgedSwift, BridgedSwift>):
func testDictionaryUpcastBridged(dict: Dictionary<BridgedSwift, BridgedSwift>) {
  // CHECK: [[BRIDGE_FN:%[0-9]+]] = function_ref @_TFSs29_dictionaryBridgeToObjectiveCUSs8Hashable__S____FGVSs10DictionaryQ_Q0__GS0_Q1_Q2__ : $@thin <τ_0_0, τ_0_1, τ_0_2, τ_0_3 where τ_0_0 : Hashable, τ_0_2 : Hashable> (@owned Dictionary<τ_0_0, τ_0_1>) -> @owned Dictionary<τ_0_2, τ_0_3>
  // CHECK-NEXT: apply [[BRIDGE_FN]]<BridgedSwift, BridgedSwift, NSObject, AnyObject>([[DICT]]) : $@thin <τ_0_0, τ_0_1, τ_0_2, τ_0_3 where τ_0_0 : Hashable, τ_0_2 : Hashable> (@owned Dictionary<τ_0_0, τ_0_1>) -> @owned Dictionary<τ_0_2, τ_0_3>
  let anyObjectDict: Dictionary<NSObject, AnyObject> = dict  
}

