// RUN: rm -rf %t && mkdir %t
// RUN: %target-swift-frontend -emit-module -enable-resilience -emit-module-path=%t/resilient_struct.swiftmodule -module-name=resilient_struct %S/../Inputs/resilient_struct.swift
// RUN: %target-swift-frontend -emit-module -enable-resilience -emit-module-path=%t/resilient_enum.swiftmodule -module-name=resilient_enum -I %t %S/../Inputs/resilient_enum.swift
// RUN: %target-swift-frontend -I %t -emit-ir -enable-resilience %s | %FileCheck %s
// RUN: %target-swift-frontend -I %t -emit-ir -enable-resilience -O %s

import resilient_struct
import resilient_enum

// CHECK: %TSi = type <{ [[INT:i32|i64]] }>

// CHECK-LABEL: @_T017struct_resilience26StructWithResilientStorageVMf = internal global

// Resilient structs from outside our resilience domain are manipulated via
// value witnesses

// CHECK-LABEL: define{{( protected)?}} swiftcc void @_T017struct_resilience26functionWithResilientTypes010resilient_A04SizeVAE_A2Ec1ftF(%swift.opaque* noalias nocapture sret, %swift.opaque* noalias nocapture, i8*, %swift.refcounted*)

public func functionWithResilientTypes(_ s: Size, f: (Size) -> Size) -> Size {

// CHECK: [[METADATA:%.*]] = call %swift.type* @_T016resilient_struct4SizeVMa()
// CHECK: [[METADATA_ADDR:%.*]] = bitcast %swift.type* [[METADATA]] to i8***
// CHECK: [[VWT_ADDR:%.*]] = getelementptr inbounds i8**, i8*** [[METADATA_ADDR]], [[INT]] -1
// CHECK: [[VWT:%.*]] = load i8**, i8*** [[VWT_ADDR]]

// CHECK: [[WITNESS_ADDR:%.*]] = getelementptr inbounds i8*, i8** [[VWT]], i32 17
// CHECK: [[WITNESS:%.*]] = load i8*, i8** [[WITNESS_ADDR]]
// CHECK: [[WITNESS_FOR_SIZE:%.*]] = ptrtoint i8* [[WITNESS]]
// CHECK: [[ALLOCA:%.*]] = alloca i8, {{.*}} [[WITNESS_FOR_SIZE]], align 16
// CHECK: [[STRUCT_ADDR:%.*]] = bitcast i8* [[ALLOCA]] to %swift.opaque*

// CHECK: [[WITNESS_PTR:%.*]] = getelementptr inbounds i8*, i8** [[VWT]], i32 6
// CHECK: [[WITNESS:%.*]] = load i8*, i8** [[WITNESS_PTR]]
// CHECK: [[initializeWithCopy:%.*]] = bitcast i8* [[WITNESS]]
// CHECK: [[STRUCT_LOC:%.*]] = call %swift.opaque* [[initializeWithCopy]](%swift.opaque* [[STRUCT_ADDR]], %swift.opaque* %1, %swift.type* [[METADATA]])

// CHECK: [[FN:%.*]] = bitcast i8* %2 to void (%swift.opaque*, %swift.opaque*, %swift.refcounted*)*
// CHECK: call swiftcc void [[FN]](%swift.opaque* noalias nocapture sret %0, %swift.opaque* noalias nocapture [[STRUCT_ADDR]], %swift.refcounted* swiftself %3)

// CHECK: [[WITNESS_PTR:%.*]] = getelementptr inbounds i8*, i8** [[VWT]], i32 4
// CHECK: [[WITNESS:%.*]] = load i8*, i8** [[WITNESS_PTR]]
// CHECK: [[destroy:%.*]] = bitcast i8* [[WITNESS]] to void (%swift.opaque*, %swift.type*)*
// CHECK: call void [[destroy]](%swift.opaque* %1, %swift.type* [[METADATA]])
// CHECK: ret void

  return f(s)
}

// CHECK-LABEL: declare %swift.type* @_T016resilient_struct4SizeVMa()

// Rectangle has fixed layout inside its resilience domain, and dynamic
// layout on the outside.
//
// Make sure we use a type metadata accessor function, and load indirect
// field offsets from it.

// CHECK-LABEL: define{{( protected)?}} swiftcc void @_T017struct_resilience26functionWithResilientTypesy010resilient_A09RectangleVF(%T16resilient_struct9RectangleV* noalias nocapture)
public func functionWithResilientTypes(_ r: Rectangle) {

// CHECK: [[METADATA:%.*]] = call %swift.type* @_T016resilient_struct9RectangleVMa()
// CHECK-NEXT: [[METADATA_ADDR:%.*]] = bitcast %swift.type* [[METADATA]] to [[INT]]*
// CHECK-NEXT: [[FIELD_OFFSET_VECTOR:%.*]] = getelementptr inbounds [[INT]], [[INT]]* [[METADATA_ADDR]], i32 3
// CHECK-NEXT: [[FIELD_OFFSET_PTR:%.*]] = getelementptr inbounds [[INT]], [[INT]]* [[FIELD_OFFSET_VECTOR]], i32 2
// CHECK-NEXT: [[FIELD_OFFSET:%.*]] = load [[INT]], [[INT]]* [[FIELD_OFFSET_PTR]]
// CHECK-NEXT: [[STRUCT_ADDR:%.*]] = bitcast %T16resilient_struct9RectangleV* %0 to i8*
// CHECK-NEXT: [[FIELD_ADDR:%.*]] = getelementptr inbounds i8, i8* [[STRUCT_ADDR]], [[INT]] [[FIELD_OFFSET]]
// CHECK-NEXT: [[FIELD_PTR:%.*]] = bitcast i8* [[FIELD_ADDR]] to %TSi*
// CHECK-NEXT: [[FIELD_PAYLOAD_PTR:%.*]] = getelementptr inbounds %TSi, %TSi* [[FIELD_PTR]], i32 0, i32 0
// CHECK-NEXT: [[FIELD_PAYLOAD:%.*]] = load [[INT]], [[INT]]* [[FIELD_PAYLOAD_PTR]]

  _ = r.color

// CHECK: ret void

}


// Resilient structs from inside our resilience domain are manipulated
// directly.

public struct MySize {
  public let w: Int
  public let h: Int
}

// CHECK-LABEL: define{{( protected)?}} swiftcc void @_T017struct_resilience28functionWithMyResilientTypesAA0E4SizeVAD_A2Dc1ftF(%T17struct_resilience6MySizeV* {{.*}}, %T17struct_resilience6MySizeV* {{.*}}, i8*, %swift.refcounted*)
public func functionWithMyResilientTypes(_ s: MySize, f: (MySize) -> MySize) -> MySize {

// CHECK: [[TEMP:%.*]] = alloca %T17struct_resilience6MySizeV
// CHECK: bitcast
// CHECK: llvm.lifetime.start
// CHECK: [[COPY:%.*]] = bitcast %T17struct_resilience6MySizeV* %4 to i8*
// CHECK: [[ARG:%.*]] = bitcast %T17struct_resilience6MySizeV* %1 to i8*
// CHECK: call void @llvm.memcpy{{.*}}(i8* [[COPY]], i8* [[ARG]], {{i32 8|i64 16}}, i32 {{.*}}, i1 false)
// CHECK: [[FN:%.*]] = bitcast i8* %2
// CHECK: call swiftcc void [[FN]](%T17struct_resilience6MySizeV* {{.*}} %0, {{.*}} [[TEMP]], %swift.refcounted* swiftself %3)
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

// CHECK-LABEL: define{{( protected)?}} swiftcc {{i32|i64}} @_T017struct_resilience26StructWithResilientStorageV1nSifg(%T17struct_resilience26StructWithResilientStorageV* {{.*}})
// CHECK: [[METADATA:%.*]] = call %swift.type* @_T017struct_resilience26StructWithResilientStorageVMa()
// CHECK-NEXT: [[METADATA_ADDR:%.*]] = bitcast %swift.type* [[METADATA]] to [[INT]]*
// CHECK-NEXT: [[FIELD_OFFSET_VECTOR:%.*]] = getelementptr inbounds [[INT]], [[INT]]* [[METADATA_ADDR]], i32 3
// CHECK-NEXT: [[FIELD_OFFSET_PTR:%.*]] = getelementptr inbounds [[INT]], [[INT]]* [[FIELD_OFFSET_VECTOR]], i32 2
// CHECK-NEXT: [[FIELD_OFFSET:%.*]] = load [[INT]], [[INT]]* [[FIELD_OFFSET_PTR]]
// CHECK-NEXT: [[STRUCT_ADDR:%.*]] = bitcast %T17struct_resilience26StructWithResilientStorageV* %0 to i8*
// CHECK-NEXT: [[FIELD_ADDR:%.*]] = getelementptr inbounds i8, i8* [[STRUCT_ADDR]], [[INT]] [[FIELD_OFFSET]]
// CHECK-NEXT: [[FIELD_PTR:%.*]] = bitcast i8* [[FIELD_ADDR]] to %TSi*
// CHECK-NEXT: [[FIELD_PAYLOAD_PTR:%.*]] = getelementptr inbounds %TSi, %TSi* [[FIELD_PTR]], i32 0, i32 0
// CHECK-NEXT: [[FIELD_PAYLOAD:%.*]] = load [[INT]], [[INT]]* [[FIELD_PAYLOAD_PTR]]
// CHECK-NEXT: ret [[INT]] [[FIELD_PAYLOAD]]


// Indirect enums with resilient payloads are still fixed-size.

public struct StructWithIndirectResilientEnum {
  public let s: FunnyShape
  public let n: Int
}


// CHECK-LABEL: define{{( protected)?}} swiftcc {{i32|i64}} @_T017struct_resilience31StructWithIndirectResilientEnumV1nSifg(%T17struct_resilience31StructWithIndirectResilientEnumV* {{.*}})
// CHECK: [[FIELD_PTR:%.*]] = getelementptr inbounds %T17struct_resilience31StructWithIndirectResilientEnumV, %T17struct_resilience31StructWithIndirectResilientEnumV* %0, i32 0, i32 1
// CHECK-NEXT: [[FIELD_PAYLOAD_PTR:%.*]] = getelementptr inbounds %TSi, %TSi* [[FIELD_PTR]], i32 0, i32 0
// CHECK-NEXT: [[FIELD_PAYLOAD:%.*]] = load [[INT]], [[INT]]* [[FIELD_PAYLOAD_PTR]]
// CHECK-NEXT: ret [[INT]] [[FIELD_PAYLOAD]]


// Partial application of methods on resilient value types

public struct ResilientStructWithMethod {
  public func method() {}
}

// Corner case -- type is address-only in SIL, but empty in IRGen

// CHECK-LABEL: define{{( protected)?}} swiftcc void @_T017struct_resilience29partialApplyOfResilientMethodyAA0f10StructWithG0V1r_tF(%T17struct_resilience25ResilientStructWithMethodV* noalias nocapture)
public func partialApplyOfResilientMethod(r: ResilientStructWithMethod) {
  _ = r.method
}

// Type is address-only in SIL, and resilient in IRGen

// CHECK-LABEL: define{{( protected)?}} swiftcc void @_T017struct_resilience29partialApplyOfResilientMethody010resilient_A04SizeV1s_tF(%swift.opaque* noalias nocapture)
public func partialApplyOfResilientMethod(s: Size) {
  _ = s.method
}

// Public metadata accessor for our resilient struct

// CHECK-LABEL: define{{( protected)?}} %swift.type* @_T017struct_resilience6MySizeVMa()
// CHECK: ret %swift.type* bitcast ([[INT]]* getelementptr inbounds {{.*}} @_T017struct_resilience6MySizeVMf, i32 0, i32 1) to %swift.type*)


// CHECK-LABEL: define{{( protected)?}} private void @initialize_metadata_StructWithResilientStorage(i8*)
// CHECK: [[FIELDS:%.*]] = alloca [4 x i8**]
// CHECK: [[VWT:%.*]] = load i8**, i8*** getelementptr inbounds ({{.*}} @_T017struct_resilience26StructWithResilientStorageVMf{{.*}}, [[INT]] -1)

// CHECK: [[FIELDS_ADDR:%.*]] = getelementptr inbounds [4 x i8**], [4 x i8**]* [[FIELDS]], i32 0, i32 0

// public let s: Size

// CHECK: [[FIELD_1:%.*]] = getelementptr inbounds i8**, i8*** [[FIELDS_ADDR]], i32 0
// CHECK: store i8** [[SIZE_AND_ALIGNMENT:%.*]], i8*** [[FIELD_1]]

// public let ss: (Size, Size)

// CHECK: [[FIELD_2:%.*]] = getelementptr inbounds i8**, i8*** [[FIELDS_ADDR]], i32 1
// CHECK: store i8** [[SIZE_AND_ALIGNMENT:%.*]], i8*** [[FIELD_2]]

// Fixed-layout aggregate -- we can reference a static value witness table
// public let n: Int

// CHECK: [[FIELD_3:%.*]] = getelementptr inbounds i8**, i8*** [[FIELDS_ADDR]], i32 2
// CHECK: store i8** getelementptr inbounds (i8*, i8** @_T0Bi{{32|64}}_WV, i32 {{.*}}), i8*** [[FIELD_3]]

// Resilient aggregate with one field -- make sure we don't look inside it
// public let i: ResilientInt
// CHECK: [[FIELD_4:%.*]] = getelementptr inbounds i8**, i8*** [[FIELDS_ADDR]], i32 3
// CHECK: store i8** [[SIZE_AND_ALIGNMENT:%.*]], i8*** [[FIELD_4]]

// CHECK: call void @swift_initStructMetadata_UniversalStrategy([[INT]] 4, i8*** [[FIELDS_ADDR]], [[INT]]* {{.*}}, i8** [[VWT]])
// CHECK: store atomic %swift.type* {{.*}} @_T017struct_resilience26StructWithResilientStorageVMf{{.*}}, %swift.type** @_T017struct_resilience26StructWithResilientStorageVML release,
// CHECK: ret void
