// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -module-cache-path=%t/clang-module-cache -sdk=%S/Inputs %s -emit-sil | FileCheck %s

import gizmo

protocol [class_protocol, objc] NSRuncing {
  func runce() -> NSObject
  func copyRuncing() -> NSObject
}

// CHECK: sil @_T14objc_protocols12objc_genericUS_9NSRuncing__FT1xQ__TCSo8NSObjectS1__
func objc_generic<T:NSRuncing>(x:T) -> (NSObject, NSObject) {
  return (x.runce(), x.copyRuncing())
  // -- Result of runce is retain_autoreleased according to default objc conv
  // CHECK: [[METHOD:%.*]] = archetype_method {{\$.*}}, @runce.1.objc
  // CHECK: [[RESULT1:%.*]] = apply [[METHOD]]([[THIS1:%.*]]) : $[cc(objc_method), thin] (T, ()) -> NSObject
  // CHECK: retain_autoreleased [[RESULT1]] : $NSObject

  // -- Result of copyRuncing is retain_autoreleased according to -copy family
  // CHECK: [[METHOD:%.*]] = archetype_method {{\$.*}}, @copyRuncing.1.objc
  // CHECK: [[RESULT2:%.*]] = apply [[METHOD]]([[THIS2:%.*]]) : $[cc(objc_method), thin] (T, ()) -> NSObject
  // CHECK-NOT: retain_autoreleased

  // -- Arguments are not consumed by objc calls
  // CHECK: release [[THIS2]]
  // CHECK: release [[THIS1]]
}

// CHECK: sil @_T14objc_protocols13objc_protocolFT1xPS_9NSRuncing__TCSo8NSObjectS1__
func objc_protocol(x:NSRuncing) -> (NSObject, NSObject) {
  return (x.runce(), x.copyRuncing())
  // -- Result of runce is retain_autoreleased according to default objc conv
  // CHECK: [[THIS1:%.*]] = project_existential_ref [[THIS1_ORIG:%.*]] :
  // CHECK: [[METHOD:%.*]] = protocol_method [[THIS1_ORIG]], @runce.1.objc
  // CHECK: [[RESULT1:%.*]] = apply [[METHOD]]([[THIS1]]) : $[cc(objc_method), thin] (Builtin.ObjCPointer, ()) -> NSObject
  // CHECK: retain_autoreleased [[RESULT1]] : $NSObject

  // -- Result of copyRuncing is retain_autoreleased according to -copy family
  // CHECK: [[THIS2:%.*]] = project_existential_ref [[THIS2_ORIG:%.*]] :
  // CHECK: [[METHOD:%.*]] = protocol_method [[THIS2_ORIG]], @copyRuncing.1.objc
  // CHECK: [[RESULT2:%.*]] = apply [[METHOD]]([[THIS2:%.*]]) : $[cc(objc_method), thin] (Builtin.ObjCPointer, ()) -> NSObject
  // CHECK-NOT: retain_autoreleased

  // -- Arguments are not consumed by objc calls
  // CHECK: release [[THIS2_ORIG]]
  // CHECK: release [[THIS1_ORIG]]
}
