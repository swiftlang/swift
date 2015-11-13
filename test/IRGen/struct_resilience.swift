// RUN: %target-swift-frontend -I %S/../Inputs -enable-source-import -emit-ir -enable-resilience %s | FileCheck %s

import resilient_struct

// Resilient structs from outside our resilience domain are manipulated via
// value witnesses

// CHECK-LABEL: define void @_TF17struct_resilience26functionWithResilientTypesFTV16resilient_struct4Size1fFS1_S1__S1_(%swift.opaque* noalias nocapture sret, %swift.opaque* noalias nocapture, i8*, %swift.refcounted*)

public func functionWithResilientTypes(s: Size, f: Size -> Size) -> Size {

// CHECK: [[RESULT:%.*]] = alloca [[BUFFER_TYPE:\[.* x i8\]]]
// CHECK: [[VWT:%.*]] = load i8**, i8*** getelementptr inbounds (i8**, i8*** bitcast (%swift.type* @_TMV16resilient_struct4Size to i8***), {{i32|i64}} -1)

// CHECK: [[WITNESS_PTR:%.*]] = getelementptr inbounds i8*, i8** [[VWT]], i32 5
// CHECK: [[WITNESS:%.*]] = load i8*, i8** [[WITNESS_PTR]]
// CHECK: [[initializeBufferWithCopy:%.*]] = bitcast i8* [[WITNESS]]
// CHECK: [[BUFFER:%.*]] = call %swift.opaque* [[initializeBufferWithCopy]]([[BUFFER_TYPE]]* [[RESULT]], %swift.opaque* %1, %swift.type* @_TMV16resilient_struct4Size)

// CHECK: [[FN:%.*]] = bitcast i8* %2 to void (%swift.opaque*, %swift.opaque*, %swift.refcounted*)*
// CHECK: call void [[FN]](%swift.opaque* noalias nocapture sret %0, %swift.opaque* noalias nocapture [[BUFFER]], %swift.refcounted* %3)

// CHECK: [[WITNESS_PTR:%.*]] = getelementptr inbounds i8*, i8** [[VWT]], i32 3
// CHECK: [[WITNESS:%.*]] = load i8*, i8** [[WITNESS_PTR]]
// CHECK: [[deallocateBuffer:%.*]] = bitcast i8* [[WITNESS]]
// CHECK: call void [[deallocateBuffer]]([[BUFFER_TYPE]]* [[RESULT]], %swift.type* @_TMV16resilient_struct4Size)

// CHECK: [[WITNESS_PTR:%.*]] = getelementptr inbounds i8*, i8** [[VWT]], i32 4
// CHECK: [[WITNESS:%.*]] = load i8*, i8** [[WITNESS_PTR]]
// CHECK: [[destroy:%.*]] = bitcast i8* [[WITNESS]] to void (%swift.opaque*, %swift.type*)*
// CHECK: call void [[destroy]](%swift.opaque* %1, %swift.type* @_TMV16resilient_struct4Size)
// CHECK: ret void

  return f(s)
}

// Resilient structs from inside our resilience domain are manipulated
// directly

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
