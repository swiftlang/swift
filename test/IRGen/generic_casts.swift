// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -target x86_64-apple-darwin10 -module-cache-path %t/clang-module-cache -sdk %S/Inputs -I=%S/Inputs -enable-source-import %s -emit-ir | FileCheck %s

import Foundation
import gizmo

// -- Protocol records for cast-to ObjC protocols
// CHECK: @_PROTOCOL__TtP13generic_casts10ObjCProto1_ = private constant
// CHECK: @"\01l_OBJC_LABEL_PROTOCOL_$__TtP13generic_casts10ObjCProto1_" = weak hidden global i8* bitcast ({{.*}} @_PROTOCOL__TtP13generic_casts10ObjCProto1_ to i8*), section "__DATA,__objc_protolist,coalesced,no_dead_strip"
// CHECK: @"\01l_OBJC_PROTOCOL_REFERENCE_$__TtP13generic_casts10ObjCProto1_" = weak hidden global i8* bitcast ({{.*}} @_PROTOCOL__TtP13generic_casts10ObjCProto1_ to i8*), section "__DATA,__objc_protorefs,coalesced,no_dead_strip"

// CHECK: @_PROTOCOL_NSRuncing = private constant
// CHECK: @"\01l_OBJC_LABEL_PROTOCOL_$_NSRuncing" = weak hidden global i8* bitcast ({{.*}} @_PROTOCOL_NSRuncing to i8*), section "__DATA,__objc_protolist,coalesced,no_dead_strip"
// CHECK: @"\01l_OBJC_PROTOCOL_REFERENCE_$_NSRuncing" = weak hidden global i8* bitcast ({{.*}} @_PROTOCOL_NSRuncing to i8*), section "__DATA,__objc_protorefs,coalesced,no_dead_strip"

// CHECK: @_PROTOCOL_PROTOCOLS__TtP13generic_casts10ObjCProto2_ = private constant { i64, [1 x i8*] } {
// CHECK:   i64 1, 
// CHECK:   @_PROTOCOL__TtP13generic_casts10ObjCProto1_
// CHECK: }

// CHECK: @_PROTOCOLS__TtC13generic_casts10ObjCClass2 = private constant { i64, [1 x i8*] } {
// CHECK:   i64 1, 
// CHECK:   @_PROTOCOL__TtP13generic_casts10ObjCProto2_
// CHECK: }

// CHECK: @_DATA__TtC13generic_casts10ObjCClass2 = private constant {{.*}} @_PROTOCOLS__TtC13generic_casts10ObjCClass2

// CHECK: define i64 @_TF13generic_casts8allToInt{{.*}}(%swift.opaque* noalias, %swift.type* %T)
func allToInt<T>(x: T) -> Int {
  return (x as Int)!
  // CHECK: [[METADATA:%.*]] = bitcast %swift.type* %T to i8*{{$}}
  // CHECK: [[CAST:%.*]] = call %swift.opaque* @swift_dynamicCastIndirectUnconditional(%swift.opaque* {{%.*}}, i8* [[METADATA]], i8* bitcast ({{.*}}@_TMdSi{{.*}}))
  // CHECK: [[INT_RESULT_PTR:%.*]] = bitcast %swift.opaque* [[CAST]] to %Si*
  // CHECK: [[INT_RESULT_PTR_0:%.*]] = getelementptr inbounds %Si* [[INT_RESULT_PTR]], i32 0, i32 0
  // CHECK: [[INT_RESULT:%.*]] = load i64* [[INT_RESULT_PTR_0]], align 8
  // CHECK: ret i64 [[INT_RESULT]]
}

// CHECK: define void @_TF13generic_casts8intToAll{{.*}}(%swift.opaque* noalias sret, i64, %swift.type* %T) {
func intToAll<T>(x: Int) -> T {
  // CHECK: [[METADATA:%.*]] = bitcast %swift.type* %T to i8*
  // CHECK: [[CAST:%.*]] = call %swift.opaque* @swift_dynamicCastIndirectUnconditional(%swift.opaque* {{%.*}}, i8* bitcast ({{.*}} @_TMdSi, {{.*}}), i8* [[METADATA]])
  return (x as T)!
}

// CHECK: define i64 @_TF13generic_casts8anyToInt{{.*}}(%"protocol<>"* noalias)
func anyToInt(x: protocol<>) -> Int {
  return (x as Int)!
}

@class_protocol @objc protocol ObjCProto1 {
  class func forClass()
  class func forInstance()

  var prop: NSObject { get }
}

@class_protocol @objc protocol ObjCProto2 : ObjCProto1 {}

@objc class ObjCClass {}

// CHECK: define %objc_object* @_TF13generic_casts9protoCast{{.*}}(%C13generic_casts9ObjCClass*) {
func protoCast(x: ObjCClass) -> protocol<ObjCProto1, NSRuncing> {
  // CHECK: load i8** @"\01l_OBJC_PROTOCOL_REFERENCE_$__TtP13generic_casts10ObjCProto1_"
  // CHECK: load i8** @"\01l_OBJC_PROTOCOL_REFERENCE_$_NSRuncing"
  // CHECK: call %objc_object* @swift_dynamicCastObjCProtocolUnconditional(%objc_object* {{%.*}}, i64 2, i8** {{%.*}})
  return (x as protocol<ObjCProto1, NSRuncing>)!
}

@objc class ObjCClass2 : NSObject, ObjCProto2 {
  class func forClass() {}
  class func forInstance() {}

  var prop: NSObject { return self }
}

// <rdar://problem/15313840>
// Class existential to opaque archetype cast
// CHECK: define void @_TF13generic_casts33classExistentialToOpaqueArchetype{{.*}}(%swift.opaque* noalias sret, %objc_object*, %swift.type* %T)
func classExistentialToOpaqueArchetype<T>(var x: ObjCProto1) -> T {
  // CHECK: [[X:%.*]] = alloca %P13generic_casts10ObjCProto1_
  // CHECK: [[LOCAL:%.*]] = alloca %P13generic_casts10ObjCProto1_
  // CHECK: [[PROTO_VALUE_ADDR:%.*]] = getelementptr inbounds %P13generic_casts10ObjCProto1_* [[LOCAL]], i32 0, i32 0
  // CHECK: [[PROTO_VALUE_ADDR:%.*]] = getelementptr inbounds %P13generic_casts10ObjCProto1_* [[LOCAL]], i32 0, i32 0
  // CHECK: [[PROTO_VALUE:%.*]] = load %objc_object** [[PROTO_VALUE_ADDR]], align 8
  // CHECK: [[PROTO_TYPE:%.*]] = call %swift.type* @swift_getObjectType(%objc_object* [[PROTO_VALUE]])
  // CHECK: [[PROTO_VALUE_OPAQUE:%.*]] = bitcast %objc_object** [[PROTO_VALUE_ADDR]] to %swift.opaque*
  // CHECK: [[PROTO_TYPE_OPAQUE:%.*]] = bitcast %swift.type* [[PROTO_TYPE]] to i8*
  // CHECK: [[T_OPAQUE:%.*]] = bitcast %swift.type* %T to i8*
  // CHECK: call %swift.opaque* @swift_dynamicCastIndirectUnconditional(%swift.opaque* [[PROTO_VALUE_OPAQUE]], i8* [[PROTO_TYPE_OPAQUE]], i8* [[T_OPAQUE]])
  return (x as T)!
}
