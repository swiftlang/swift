
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -enable-resilience -emit-module-path=%t/resilient_struct.swiftmodule -module-name=resilient_struct %S/../Inputs/resilient_struct.swift
// RUN: %target-swift-frontend -emit-module -enable-resilience -emit-module-path=%t/resilient_enum.swiftmodule -module-name=resilient_enum -I %t %S/../Inputs/resilient_enum.swift
// RUN: %target-swift-frontend -module-name struct_resilience -I %t -emit-ir -enable-resilience %s | %FileCheck %s
// RUN: %target-swift-frontend -module-name struct_resilience -I %t -emit-ir -enable-resilience -O %s

import resilient_struct
import resilient_enum

// CHECK: %TSi = type <{ [[INT:i32|i64]] }>

// CHECK-LABEL: @"$S17struct_resilience26StructWithResilientStorageVMf" = internal global

// Resilient structs from outside our resilience domain are manipulated via
// value witnesses

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc void @"$S17struct_resilience30functionWithResilientTypesSize_1f010resilient_A00G0VAFn_A2FnXEtF"(%swift.opaque* noalias nocapture sret, %swift.opaque* noalias nocapture, i8*, %swift.opaque*)

public func functionWithResilientTypesSize(_ s: __owned Size, f: (__owned Size) -> Size) -> Size {
// CHECK: entry:
// CHECK: [[TMP:%.*]] = call swiftcc %swift.metadata_response @"$S16resilient_struct4SizeVMa"([[INT]] 0)
// CHECK: [[METADATA:%.*]] = extractvalue %swift.metadata_response [[TMP]], 0
// CHECK: [[METADATA_ADDR:%.*]] = bitcast %swift.type* [[METADATA]] to i8***
// CHECK: [[VWT_ADDR:%.*]] = getelementptr inbounds i8**, i8*** [[METADATA_ADDR]], [[INT]] -1
// CHECK: [[VWT:%.*]] = load i8**, i8*** [[VWT_ADDR]]

// CHECK-NEXT: [[WITNESS_ADDR:%.*]] = getelementptr inbounds i8*, i8** [[VWT]], i32 8
// CHECK: [[WITNESS:%.*]] = load i8*, i8** [[WITNESS_ADDR]]
// CHECK: [[WITNESS_FOR_SIZE:%.*]] = ptrtoint i8* [[WITNESS]]
// CHECK: [[ALLOCA:%.*]] = alloca i8, {{.*}} [[WITNESS_FOR_SIZE]], align 16
// CHECK: [[STRUCT_ADDR:%.*]] = bitcast i8* [[ALLOCA]] to %swift.opaque*

// CHECK: [[WITNESS_PTR:%.*]] = getelementptr inbounds i8*, i8** [[VWT]], i32 2
// CHECK: [[WITNESS:%.*]] = load i8*, i8** [[WITNESS_PTR]]
// CHECK: [[initializeWithCopy:%.*]] = bitcast i8* [[WITNESS]]
// CHECK: [[STRUCT_LOC:%.*]] = call %swift.opaque* [[initializeWithCopy]](%swift.opaque* noalias [[STRUCT_ADDR]], %swift.opaque* noalias %1, %swift.type* [[METADATA]])

// CHECK: [[FN:%.*]] = bitcast i8* %2 to void (%swift.opaque*, %swift.opaque*, %swift.refcounted*)*
// CHECK: [[SELF:%.*]] = bitcast %swift.opaque* %3 to %swift.refcounted*
// CHECK: call swiftcc void [[FN]](%swift.opaque* noalias nocapture sret %0, %swift.opaque* noalias nocapture [[STRUCT_ADDR]], %swift.refcounted* swiftself [[SELF]])

// CHECK: [[WITNESS_PTR:%.*]] = getelementptr inbounds i8*, i8** [[VWT]], i32 1
// CHECK: [[WITNESS:%.*]] = load i8*, i8** [[WITNESS_PTR]]
// CHECK: [[destroy:%.*]] = bitcast i8* [[WITNESS]] to void (%swift.opaque*, %swift.type*)*
// CHECK: call void [[destroy]](%swift.opaque* noalias %1, %swift.type* [[METADATA]])
// CHECK-NEXT: bitcast
// CHECK-NEXT: call
// CHECK-NEXT: ret void

  return f(s)
}

// CHECK-LABEL: declare{{( dllimport)?}} swiftcc %swift.metadata_response @"$S16resilient_struct4SizeVMa"
// CHECK-SAME:    ([[INT]])

// Rectangle has fixed layout inside its resilience domain, and dynamic
// layout on the outside.
//
// Make sure we use a type metadata accessor function, and load indirect
// field offsets from it.

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc void @"$S17struct_resilience35functionWithResilientTypesRectangleyy010resilient_A00G0VF"(%T16resilient_struct9RectangleV* noalias nocapture)
public func functionWithResilientTypesRectangle(_ r: Rectangle) {
// CHECK: entry:
// CHECK-NEXT: [[TMP:%.*]] = call swiftcc %swift.metadata_response @"$S16resilient_struct9RectangleVMa"([[INT]] 0)
// CHECK-NEXT: [[METADATA:%.*]] = extractvalue %swift.metadata_response [[TMP]], 0
// CHECK-NEXT: [[METADATA_ADDR:%.*]] = bitcast %swift.type* [[METADATA]] to i32*
// CHECK-NEXT: [[FIELD_OFFSET_VECTOR:%.*]] = getelementptr inbounds i32, i32* [[METADATA_ADDR]], [[INT]] [[IDX:2|4]]
// CHECK-NEXT: [[FIELD_OFFSET_PTR:%.*]] = getelementptr inbounds i32, i32* [[FIELD_OFFSET_VECTOR]], i32 2
// CHECK-NEXT: [[FIELD_OFFSET:%.*]] = load i32, i32* [[FIELD_OFFSET_PTR]]
// CHECK-NEXT: [[STRUCT_ADDR:%.*]] = bitcast %T16resilient_struct9RectangleV* %0 to i8*
// CHECK-NEXT: [[FIELD_ADDR:%.*]] = getelementptr inbounds i8, i8* [[STRUCT_ADDR]], i32 [[FIELD_OFFSET]]
// CHECK-NEXT: [[FIELD_PTR:%.*]] = bitcast i8* [[FIELD_ADDR]] to %TSi*
// CHECK-NEXT: [[FIELD_PAYLOAD_PTR:%.*]] = getelementptr inbounds %TSi, %TSi* [[FIELD_PTR]], i32 0, i32 0
// CHECK-NEXT: [[FIELD_PAYLOAD:%.*]] = load [[INT]], [[INT]]* [[FIELD_PAYLOAD_PTR]]

  _ = r.color

// CHECK-NEXT: ret void

}


// Resilient structs from inside our resilience domain are manipulated
// directly.

public struct MySize {
  public let w: Int
  public let h: Int
}

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc void @"$S17struct_resilience32functionWithMyResilientTypesSize_1fAA0eH0VAEn_A2EnXEtF"(%T17struct_resilience6MySizeV* noalias nocapture sret, %T17struct_resilience6MySizeV* noalias nocapture dereferenceable({{8|(16)}}), i8*, %swift.opaque*)
public func functionWithMyResilientTypesSize(_ s: __owned MySize, f: (__owned MySize) -> MySize) -> MySize {
// CHECK: alloca
// CHECK: [[TEMP:%.*]] = alloca %T17struct_resilience6MySizeV
// CHECK: bitcast
// CHECK: llvm.lifetime.start
// CHECK: bitcast
// CHECK: [[COPY:%.*]] = bitcast %T17struct_resilience6MySizeV* %4 to i8*
// CHECK: [[ARG:%.*]] = bitcast %T17struct_resilience6MySizeV* %1 to i8*
// CHECK: call void @llvm.memcpy{{.*}}(i8* [[COPY]], i8* [[ARG]], {{i32 8|i64 16}}, i32 {{.*}}, i1 false)
// CHECK: [[FN:%.*]] = bitcast i8* %2
// CHECK: call swiftcc void [[FN]](%T17struct_resilience6MySizeV* {{.*}} %0, {{.*}} [[TEMP]], %swift.refcounted* swiftself %{{.*}})
// CHECK: ret void

  return f(s)
}

// Structs with resilient storage from a different resilience domain require
// runtime metadata instantiation, just like generics.

public struct StructWithResilientStorage {
  public let s: Size
  public let ss: (Size, Size)
  public let n: Int
  public let i: ResilientInt
}

// Make sure we call a function to access metadata of structs with
// resilient layout, and go through the field offset vector in the
// metadata when accessing stored properties.

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc {{i32|i64}} @"$S17struct_resilience26StructWithResilientStorageV1nSivg"(%T17struct_resilience26StructWithResilientStorageV* {{.*}})
// CHECK: [[TMP:%.*]] = call swiftcc %swift.metadata_response @"$S17struct_resilience26StructWithResilientStorageVMa"([[INT]] 0)
// CHECK: [[METADATA:%.*]] = extractvalue %swift.metadata_response [[TMP]], 0
// CHECK-NEXT: [[METADATA_ADDR:%.*]] = bitcast %swift.type* [[METADATA]] to i32*
// CHECK-NEXT: [[FIELD_OFFSET_VECTOR:%.*]] = getelementptr inbounds i32, i32* [[METADATA_ADDR]], [[INT]] [[IDX:2|4]]
// CHECK-NEXT: [[FIELD_OFFSET_PTR:%.*]] = getelementptr inbounds i32, i32* [[FIELD_OFFSET_VECTOR]], i32 2
// CHECK-NEXT: [[FIELD_OFFSET:%.*]] = load i32, i32* [[FIELD_OFFSET_PTR]]
// CHECK-NEXT: [[STRUCT_ADDR:%.*]] = bitcast %T17struct_resilience26StructWithResilientStorageV* %0 to i8*
// CHECK-NEXT: [[FIELD_ADDR:%.*]] = getelementptr inbounds i8, i8* [[STRUCT_ADDR]], i32 [[FIELD_OFFSET]]
// CHECK-NEXT: [[FIELD_PTR:%.*]] = bitcast i8* [[FIELD_ADDR]] to %TSi*
// CHECK-NEXT: [[FIELD_PAYLOAD_PTR:%.*]] = getelementptr inbounds %TSi, %TSi* [[FIELD_PTR]], i32 0, i32 0
// CHECK-NEXT: [[FIELD_PAYLOAD:%.*]] = load [[INT]], [[INT]]* [[FIELD_PAYLOAD_PTR]]
// CHECK-NEXT: ret [[INT]] [[FIELD_PAYLOAD]]


// Indirect enums with resilient payloads are still fixed-size.

public struct StructWithIndirectResilientEnum {
  public let s: FunnyShape
  public let n: Int
}


// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc {{i32|i64}} @"$S17struct_resilience31StructWithIndirectResilientEnumV1nSivg"(%T17struct_resilience31StructWithIndirectResilientEnumV* {{.*}})
// CHECK: [[FIELD_PTR:%.*]] = getelementptr inbounds %T17struct_resilience31StructWithIndirectResilientEnumV, %T17struct_resilience31StructWithIndirectResilientEnumV* %0, i32 0, i32 1
// CHECK-NEXT: [[FIELD_PAYLOAD_PTR:%.*]] = getelementptr inbounds %TSi, %TSi* [[FIELD_PTR]], i32 0, i32 0
// CHECK-NEXT: [[FIELD_PAYLOAD:%.*]] = load [[INT]], [[INT]]* [[FIELD_PAYLOAD_PTR]]
// CHECK-NEXT: ret [[INT]] [[FIELD_PAYLOAD]]


// Partial application of methods on resilient value types

public struct ResilientStructWithMethod {
  public func method() {}
}

// Corner case -- type is address-only in SIL, but empty in IRGen

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc void @"$S17struct_resilience29partialApplyOfResilientMethod1ryAA0f10StructWithG0V_tF"(%T17struct_resilience25ResilientStructWithMethodV* noalias nocapture)
public func partialApplyOfResilientMethod(r: ResilientStructWithMethod) {
  _ = r.method
}

// Type is address-only in SIL, and resilient in IRGen

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc void @"$S17struct_resilience29partialApplyOfResilientMethod1sy010resilient_A04SizeV_tF"(%swift.opaque* noalias nocapture)
public func partialApplyOfResilientMethod(s: Size) {
  _ = s.method
}

public func wantsAny(_ any: Any) {}

public func resilientAny(s : ResilientWeakRef) {
  wantsAny(s)
}

// CHECK-LABEL: define{{.*}} swiftcc void @"$S17struct_resilience12resilientAny1sy0c1_A016ResilientWeakRefV_tF"(%swift.opaque* noalias nocapture)
// CHECK: entry:
// CHECK: [[ANY:%.*]] = alloca %Any
// CHECK: [[META:%.*]] = call swiftcc %swift.metadata_response @"$S16resilient_struct16ResilientWeakRefVMa"([[INT]] 0)
// CHECK: [[META2:%.*]] = extractvalue %swift.metadata_response %3, 0
// CHECK: [[TYADDR:%.*]] = getelementptr inbounds %Any, %Any* %1, i32 0, i32 1
// CHECK:  store %swift.type* [[META2]], %swift.type** [[TYADDR]]
// CHECK:  call %swift.opaque* @__swift_allocate_boxed_opaque_existential_0(%Any* [[ANY]])
// CHECK:  call swiftcc void @"$S17struct_resilience8wantsAnyyyypF"(%Any* noalias nocapture dereferenceable({{(32|16)}}) [[ANY]])
// CHECK:  call void @__swift_destroy_boxed_opaque_existential_0(%Any* [[ANY]])
// CHECK:  ret void

// Public metadata accessor for our resilient struct

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc %swift.metadata_response @"$S17struct_resilience6MySizeVMa"
// CHECK-SAME:    ([[INT]])
// CHECK: ret %swift.metadata_response { %swift.type* bitcast ([[INT]]* getelementptr inbounds {{.*}} @"$S17struct_resilience6MySizeVMf", i32 0, i32 1) to %swift.type*), [[INT]] 0 }


// CHECK-LABEL:  define internal swiftcc %swift.metadata_response @"$S17struct_resilience26StructWithResilientStorageVMr"(%swift.type*, i8*, i8**)
// CHECK: [[FIELDS:%.*]] = alloca [4 x i8**]
// CHECK: [[TUPLE_LAYOUT:%.*]] = alloca %swift.full_type_layout,

// CHECK: [[FIELDS_ADDR:%.*]] = getelementptr inbounds [4 x i8**], [4 x i8**]* [[FIELDS]], i32 0, i32 0

// public let s: Size

// CHECK: [[T0:%.*]] = call swiftcc %swift.metadata_response @"$S16resilient_struct4SizeVMa"([[INT]] 319)
// CHECK: [[SIZE_METADATA:%.*]] = extractvalue %swift.metadata_response [[T0]], 0
// CHECK: [[T0:%.*]] = bitcast %swift.type* [[SIZE_METADATA]] to i8***
// CHECK: [[T1:%.*]] = getelementptr inbounds i8**, i8*** [[T0]], [[INT]] -1
// CHECK: [[SIZE_VWT:%.*]] = load i8**, i8*** [[T1]],
// CHECK: [[SIZE_LAYOUT_1:%.*]] = getelementptr inbounds i8*, i8** [[SIZE_VWT]], i32 8
// CHECK: [[FIELD_1:%.*]] = getelementptr inbounds i8**, i8*** [[FIELDS_ADDR]], i32 0
// CHECK: store i8** [[SIZE_LAYOUT_1:%.*]], i8*** [[FIELD_1]]

// public let ss: (Size, Size)

// CHECK: [[SIZE_LAYOUT_2:%.*]] = getelementptr inbounds i8*, i8** [[SIZE_VWT]], i32 8
// CHECK: [[SIZE_LAYOUT_3:%.*]] = getelementptr inbounds i8*, i8** [[SIZE_VWT]], i32 8
// CHECK: call swiftcc [[INT]] @swift_getTupleTypeLayout2(%swift.full_type_layout* [[TUPLE_LAYOUT]], i8** [[SIZE_LAYOUT_2]], i8** [[SIZE_LAYOUT_3]])
// CHECK: [[T0:%.*]] = bitcast %swift.full_type_layout* [[TUPLE_LAYOUT]] to i8**
// CHECK: [[FIELD_2:%.*]] = getelementptr inbounds i8**, i8*** [[FIELDS_ADDR]], i32 1
// CHECK: store i8** [[T0]], i8*** [[FIELD_2]]

// Fixed-layout aggregate -- we can reference a static value witness table
// public let n: Int

// CHECK: [[FIELD_3:%.*]] = getelementptr inbounds i8**, i8*** [[FIELDS_ADDR]], i32 2
// CHECK: store i8** getelementptr inbounds (i8*, i8** @"$SBi{{32|64}}_WV", i32 {{.*}}), i8*** [[FIELD_3]]

// Resilient aggregate with one field -- make sure we don't look inside it
// public let i: ResilientInt
// CHECK: call swiftcc %swift.metadata_response @"$S16resilient_struct12ResilientIntVMa"([[INT]] 319)
// CHECK: [[FIELD_4:%.*]] = getelementptr inbounds i8**, i8*** [[FIELDS_ADDR]], i32 3
// CHECK: store i8** [[SIZE_AND_ALIGNMENT:%.*]], i8*** [[FIELD_4]]

// CHECK: call void @swift_initStructMetadata(%swift.type* {{.*}}, [[INT]] 256, [[INT]] 4, i8*** [[FIELDS_ADDR]], i32* {{.*}})
