// RUN: %target-swift-frontend -emit-ir %s -swift-version 5 -disable-availability-checking | %IRGenFileCheck %s

// CHECK-LABEL: @"$sl26existential_shape_metadata2Q0_pyxXPXGMq" = linkonce_odr hidden constant
// CHECK-SAME:  { i32 {{.*}}sub ([[INT]] ptrtoint (i8** @{{[0-9]+}} to [[INT]])
// CHECK-SAME:    i32 6400,
// CHECK-SAME:    i32 {{.*}} @"flat unique 26existential_shape_metadata2Q0_pyxXP"
// CHECK-SAME:    i16 2, i16 2, i16 3, i16 0,
// CHECK-SAME:    i16 1, i16 0, i16 1, i16 0, i32 1,

// CHECK-LABEL: @"$sl26existential_shape_metadata2R033_881A0B6978EB4286E7CFF1E27030ACACLL_pyxXPXG" = internal constant
// CHECK-SAME:  { i32 6400,
//   This could use a symbolic reference because this doesn't have to be uniqued.
// CHECK-SAME:    i32 {{.*}} @"flat unique 26existential_shape_metadata2R033_881A0B6978EB4286E7CFF1E27030ACACLL_pyxXP"
// CHECK-SAME:    i16 2, i16 2, i16 3, i16 0,
// CHECK-SAME:    i16 1, i16 0, i16 1, i16 0, i32 1,

public protocol P0 {}
public protocol P1 {}

public protocol Q0<T> {
  associatedtype T
}
public protocol Q1<A> {
  associatedtype A
}
public protocol Q2<A, B>: Q1 {
  associatedtype A
  associatedtype B
}

private protocol R0<T> {
  associatedtype T
}

public struct B<T> {}
private struct C {}

// CHECK-LABEL: define{{.*}} @"$s26existential_shape_metadata12testConcrete
public func testConcrete() -> Any.Type {
  // CHECK: [[T0:%.*]] = call swiftcc %swift.metadata_response @"$s26existential_shape_metadata2Q0_pySiXPMa"([[INT]] 0)
  // CHECK: [[METADATA:%.*]] = extractvalue %swift.metadata_response [[T0]], 0
  // CHECK: ret %swift.type* [[METADATA]]
  return (any Q0<Int>).self
}

// CHECK-LABEL: define{{.*}} linkonce_odr hidden swiftcc %swift.metadata_response @"$s26existential_shape_metadata2Q0_pySiXPMa"(
  // CHECK: [[ENTRY:.*]]:
  // CHECK:   [[ARGS:%.*]] = alloca [1 x i8*], align
  // CHECK:   [[LOAD:%.*]] = load %swift.type*, %swift.type** @"$s26existential_shape_metadata2Q0_pySiXPML", align
  // CHECK:   [[NULL:%.*]] = icmp eq %swift.type* [[LOAD]], null
  // CHECK:   br i1 [[NULL]], label %[[CACHE_IS_NULL:.*]], label %[[CONT:.*]]
  // CHECK: [[CACHE_IS_NULL]]:
  // CHECK:   [[T0:%.*]] = getelementptr inbounds [1 x i8*], [1 x i8*]* [[ARGS]], i32 0, i32 0
  // CHECK:   store i8*  bitcast (%swift.type* @"$sSiN" to i8*), i8** [[T0]], align
  // CHECK:   [[T0:%.*]] = bitcast [1 x i8*]* [[ARGS]] to i8**
  // CHECK:   [[CALL:%.*]] = call %swift.type* @swift_getExtendedExistentialTypeMetadata({{.*}}@"$sl26existential_shape_metadata2Q0_pyxXPXGMq{{(\.ptrauth)?}}" to i8*), i8** [[T0]])
  // CHECK:   store atomic %swift.type* [[CALL]], %swift.type** @"$s26existential_shape_metadata2Q0_pySiXPML" release, align
  // CHECK:   br label %[[CONT]]
  // CHECK: [[CONT]]:
  // CHECK:   [[METADATA:%.*]] = phi %swift.type* [ [[LOAD]], %[[ENTRY]] ], [ [[CALL]], %[[CACHE_IS_NULL]] ]
  // CHECK:   [[T0:%.*]] = insertvalue %swift.metadata_response undef, %swift.type* [[METADATA]], 0
  // CHECK:   [[T1:%.*]] = insertvalue %swift.metadata_response [[T0]], [[INT]] 0, 1
  // CHECK:   ret %swift.metadata_response [[T1]]

//   Still the same shape with an application of a dependent type.
// CHECK-LABEL: define{{.*}} @"$s26existential_shape_metadata13testDependent
public func testDependent<T>(t: T.Type) -> Any.Type {
  // CHECK: [[ARGS:%.*]] = alloca [1 x i8*], align
  // CHECK: [[T0:%.*]] = getelementptr inbounds [1 x i8*], [1 x i8*]* [[ARGS]], i32 0, i32 0
  // CHECK: [[T1:%.*]] = bitcast %swift.type* %T to i8*
  // CHECK: store i8* [[T1]], i8** [[T0]], align
  // CHECK: [[T0:%.*]] = bitcast [1 x i8*]* [[ARGS]] to i8**
  // CHECK: [[METADATA:%.*]] = call %swift.type* @swift_getExtendedExistentialTypeMetadata({{.*}}@"$sl26existential_shape_metadata2Q0_pyxXPXGMq{{(\.ptrauth)?}}" to i8*), i8** [[T0]])
  // CHECK: ret %swift.type* [[METADATA]]
  return (any Q0<T>).self
}

//   Still the same shape with an application of a complex type.
// CHECK-LABEL: define{{.*}} @"$s26existential_shape_metadata22testComplexApplication
public func testComplexApplication<T>(t: T.Type) -> Any.Type {
  // CHECK: [[ARGS:%.*]] = alloca [1 x i8*], align
  // CHECK: [[T0:%.*]] = call swiftcc %swift.metadata_response @"$s26existential_shape_metadata1BVMa"([[INT]] 255, %swift.type* %T)
  // CHECK: [[B_T:%.*]] = extractvalue %swift.metadata_response [[T0]], 0
  // CHECK: [[T0:%.*]] = getelementptr inbounds [1 x i8*], [1 x i8*]* [[ARGS]], i32 0, i32 0
  // CHECK: [[T1:%.*]] = bitcast %swift.type* [[B_T]] to i8*
  // CHECK: store i8* [[T1]], i8** [[T0]], align
  // CHECK: [[T0:%.*]] = bitcast [1 x i8*]* [[ARGS]] to i8**
  // CHECK: [[METADATA:%.*]] = call %swift.type* @swift_getExtendedExistentialTypeMetadata({{.*}}@"$sl26existential_shape_metadata2Q0_pyxXPXGMq{{(\.ptrauth)?}}" to i8*), i8** [[T0]])
  // CHECK: ret %swift.type* [[METADATA]]
  return (any Q0<B<T>>).self
}

//   Private protocols make the shape private and unique.
// CHECK-LABEL: define{{.*}} @"$s26existential_shape_metadata12test_private
public func test_private<T>(t: T.Type) -> Any.Type {
  // CHECK: [[ARGS:%.*]] = alloca [1 x i8*], align
  // CHECK: [[T0:%.*]] = getelementptr inbounds [1 x i8*], [1 x i8*]* [[ARGS]], i32 0, i32 0
  // CHECK: [[T1:%.*]] = bitcast %swift.type* %T to i8*
  // CHECK: store i8* [[T1]], i8** [[T0]], align
  // CHECK: [[T0:%.*]] = bitcast [1 x i8*]* [[ARGS]] to i8**
  //   FIXME: this should be unique?
  // CHECK: [[METADATA:%.*]] = call %swift.type* @swift_getExtendedExistentialTypeMetadata_unique({{.*}}@"$sl26existential_shape_metadata2R033_881A0B6978EB4286E7CFF1E27030ACACLL_pyxXPXG{{(\.ptrauth)?}}" to i8*), i8** [[T0]])
  // CHECK: ret %swift.type* [[METADATA]]
  return (any R0<T>).self
}

//   Applying private arguments to a public protocol does not
//   change anything: the shape is still public.
// CHECK-LABEL: define{{.*}} @"$s26existential_shape_metadata23test_privateApplication
public func test_privateApplication<T>(t: T.Type) -> Any.Type {
  // CHECK: [[T0:%.*]] = call swiftcc %swift.metadata_response @"$s26existential_shape_metadata2Q0_pyAA1C33_881A0B6978EB4286E7CFF1E27030ACACLLVXPMa"([[INT]] 0)
  // CHECK: [[METADATA:%.*]] = extractvalue %swift.metadata_response [[T0]], 0
  // CHECK: ret %swift.type* [[METADATA]]
  return (any Q0<C>).self
}

//   FIXME: this accessor should have internal linkage, but we compute
//   formal linkage wrong for types
// CHECK-LABEL: define{{.*}} linkonce_odr hidden swiftcc %swift.metadata_response @"$s26existential_shape_metadata2Q0_pyAA1C33_881A0B6978EB4286E7CFF1E27030ACACLLVXPMa"(
  // CHECK: [[ARGS:%.*]] = alloca [1 x i8*], align
  // CHECK: [[T0:%.*]] = getelementptr inbounds [1 x i8*], [1 x i8*]* [[ARGS]], i32 0, i32 0
  // CHECK: store i8* {{.*}}"$s26existential_shape_metadata1C33_881A0B6978EB4286E7CFF1E27030ACACLLVMf"{{.*}}, i8** [[T0]], align
  // CHECK: [[T0:%.*]] = bitcast [1 x i8*]* [[ARGS]] to i8**
  // CHECK: [[METADATA:%.*]] = call %swift.type* @swift_getExtendedExistentialTypeMetadata({{.*}}@"$sl26existential_shape_metadata2Q0_pyxXPXGMq{{(\.ptrauth)?}}" to i8*), i8** [[T0]])
