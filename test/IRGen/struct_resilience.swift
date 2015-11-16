// RUN: %target-swift-frontend -I %S/../Inputs -enable-source-import -emit-ir -enable-resilience %s | FileCheck %s
// RUN: %target-swift-frontend -I %S/../Inputs -enable-source-import -emit-ir -enable-resilience -O %s

import resilient_struct

// CHECK: %Si = type <{ [[INT:i32|i64]] }>

// Resilient structs from outside our resilience domain are manipulated via
// value witnesses

// CHECK-LABEL: define void @_TF17struct_resilience26functionWithResilientTypesFTV16resilient_struct4Size1fFS1_S1__S1_(%swift.opaque* noalias nocapture sret, %swift.opaque* noalias nocapture, i8*, %swift.refcounted*)

public func functionWithResilientTypes(s: Size, f: Size -> Size) -> Size {

// CHECK: [[RESULT:%.*]] = alloca [[BUFFER_TYPE:\[.* x i8\]]]

// CHECK: [[METADATA:%.*]] = call %swift.type* @_TMaV16resilient_struct4Size()
// CHECK: [[METADATA_ADDR:%.*]] = bitcast %swift.type* [[METADATA]] to i8***
// CHECK: [[VWT_ADDR:%.*]] = getelementptr inbounds i8**, i8*** [[METADATA_ADDR]], [[INT]] -1
// CHECK: [[VWT:%.*]] = load i8**, i8*** [[VWT_ADDR]]

// CHECK: [[WITNESS_PTR:%.*]] = getelementptr inbounds i8*, i8** [[VWT]], i32 5
// CHECK: [[WITNESS:%.*]] = load i8*, i8** [[WITNESS_PTR]]
// CHECK: [[initializeBufferWithCopy:%.*]] = bitcast i8* [[WITNESS]]
// CHECK: [[BUFFER:%.*]] = call %swift.opaque* [[initializeBufferWithCopy]]([[BUFFER_TYPE]]* [[RESULT]], %swift.opaque* %1, %swift.type* [[METADATA]])

// CHECK: [[FN:%.*]] = bitcast i8* %2 to void (%swift.opaque*, %swift.opaque*, %swift.refcounted*)*
// CHECK: call void [[FN]](%swift.opaque* noalias nocapture sret %0, %swift.opaque* noalias nocapture [[BUFFER]], %swift.refcounted* %3)

// CHECK: [[WITNESS_PTR:%.*]] = getelementptr inbounds i8*, i8** [[VWT]], i32 3
// CHECK: [[WITNESS:%.*]] = load i8*, i8** [[WITNESS_PTR]]
// CHECK: [[deallocateBuffer:%.*]] = bitcast i8* [[WITNESS]]
// CHECK: call void [[deallocateBuffer]]([[BUFFER_TYPE]]* [[RESULT]], %swift.type* [[METADATA]])

// CHECK: [[WITNESS_PTR:%.*]] = getelementptr inbounds i8*, i8** [[VWT]], i32 4
// CHECK: [[WITNESS:%.*]] = load i8*, i8** [[WITNESS_PTR]]
// CHECK: [[destroy:%.*]] = bitcast i8* [[WITNESS]] to void (%swift.opaque*, %swift.type*)*
// CHECK: call void [[destroy]](%swift.opaque* %1, %swift.type* [[METADATA]])
// CHECK: ret void

  return f(s)
}

// CHECK-LABEL: declare %swift.type* @_TMaV16resilient_struct4Size()

// Rectangle has fixed layout inside its resilience domain, and dynamic
// layout on the outside.
//
// Make sure we use a type metadata accessor function, and load indirect
// field offsets from it.

// CHECK-LABEL: define void @_TF17struct_resilience26functionWithResilientTypesFV16resilient_struct9RectangleT_(%V16resilient_struct9Rectangle* noalias nocapture)
public func functionWithResilientTypes(r: Rectangle) {

// CHECK: [[METADATA:%.*]] = call %swift.type* @_TMaV16resilient_struct9Rectangle()
// CHECK-NEXT: [[METADATA_ADDR:%.*]] = bitcast %swift.type* [[METADATA]] to [[INT]]*
// CHECK-NEXT: [[FIELD_OFFSET_VECTOR:%.*]] = getelementptr inbounds [[INT]], [[INT]]* [[METADATA_ADDR]], i32 3
// CHECK-NEXT: [[FIELD_OFFSET_PTR:%.*]] = getelementptr inbounds [[INT]], [[INT]]* [[FIELD_OFFSET_VECTOR]], i32 2
// CHECK-NEXT: [[FIELD_OFFSET:%.*]] = load [[INT]], [[INT]]* [[FIELD_OFFSET_PTR]]
// CHECK-NEXT: [[STRUCT_ADDR:%.*]] = bitcast %V16resilient_struct9Rectangle* %0 to i8*
// CHECK-NEXT: [[FIELD_ADDR:%.*]] = getelementptr inbounds i8, i8* [[STRUCT_ADDR]], [[INT]] [[FIELD_OFFSET]]
// CHECK-NEXT: [[FIELD_PTR:%.*]] = bitcast i8* [[FIELD_ADDR]] to %Si*
// CHECK-NEXT: [[FIELD_PAYLOAD_PTR:%.*]] = getelementptr inbounds %Si, %Si* [[FIELD_PTR]], i32 0, i32 0
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

// CHECK-LABEL: define void @_TF17struct_resilience28functionWithMyResilientTypesFTVS_6MySize1fFS0_S0__S0_(%V17struct_resilience6MySize* {{.*}}, %V17struct_resilience6MySize* {{.*}}, i8*, %swift.refcounted*)
public func functionWithMyResilientTypes(s: MySize, f: MySize -> MySize) -> MySize {

// CHECK: [[TEMP:%.*]] = alloca %V17struct_resilience6MySize
// CHECK: [[COPY:%.*]] = bitcast %V17struct_resilience6MySize* %4 to i8*
// CHECK: [[ARG:%.*]] = bitcast %V17struct_resilience6MySize* %1 to i8*
// CHECK: call void @llvm.memcpy{{.*}}(i8* [[COPY]], i8* [[ARG]], {{i32 8|i64 16}}, i32 {{.*}}, i1 false)
// CHECK: [[FN:%.*]] = bitcast i8* %2
// CHECK: call void [[FN]](%V17struct_resilience6MySize* {{.*}} %0, {{.*}} [[TEMP]], %swift.refcounted* %3)
// CHECK: ret void

  return f(s)
}

// Public metadata accessor for our resilient struct

// CHECK-LABEL: define %swift.type* @_TMaV17struct_resilience6MySize()
// CHECK: ret %swift.type* bitcast (i64* getelementptr inbounds {{.*}} @_TMfV17struct_resilience6MySize, i32 0, i32 1) to %swift.type*)
