// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -primary-file %s -emit-ir | %FileCheck %s --check-prefix=CHECK-%target-ptrsize

struct A<T1, T2>
{
  var b: T1
  var c: T2
  var d: B<T1, T2>
}
struct B<T1, T2>
{
  var c: T1
  var d: T2
}

struct C<T1>
{}
struct D<T2>
{}

struct Foo<A1, A2>
{
  var a: A1
  var b: Bar<A1, A2>
}

struct Bar<A1, A2> {
}

public protocol Proto { }

public struct EmptyStruct {}

public struct GenericStruct<T : Proto> {
  var empty: EmptyStruct = EmptyStruct()
  var dummy: Int = 0
  var opt: Optional<T> = nil

  public init() {}
}

// CHECK-32-LABEL: define{{.*}} swiftcc void @"$S15generic_structs13GenericStructVACyxGycfC"
// CHECK-32:  [[T0:%.*]] = call swiftcc %swift.metadata_response @"$S15generic_structs13GenericStructVMa"(i32 0, %swift.type* %T, i8** %T.Proto)
// CHECK-32:  [[TYPE:%.*]] = extractvalue %swift.metadata_response [[T0]], 0
// CHECK-32:  [[PTR:%.*]] = bitcast %swift.type* [[TYPE]] to i32*
// CHECK-32:  [[FIELDOFFSETS:%.*]] = getelementptr inbounds i32, i32* [[PTR]], i32 4
// CHECK-32:  [[FIELDOFFSET:%.*]] = getelementptr inbounds i32, i32* [[FIELDOFFSETS]], i32 2
// CHECK-32:  [[OFFSET:%.*]] = load i32, i32* [[FIELDOFFSET]]
// CHECK-32:  [[ADDROFOPT:%.*]] = getelementptr inbounds i8, i8* {{.*}}, i32 [[OFFSET]]
// CHECK-32:  [[OPTPTR:%.*]] = bitcast i8* [[ADDROFOPT]] to %TSq*
// CHECK-32:  call %TSq* @"$S15generic_structsytWb3_"(%TSq* {{.*}}, %TSq* [[OPTPTR]]

// CHECK-64-LABEL: define{{.*}} swiftcc void @"$S15generic_structs13GenericStructVACyxGycfC"
// CHECK-64:  [[T0:%.*]] = call swiftcc %swift.metadata_response @"$S15generic_structs13GenericStructVMa"(i64 0, %swift.type* %T, i8** %T.Proto)
// CHECK-64:  [[TYPE:%.*]] = extractvalue %swift.metadata_response [[T0]], 0
// CHECK-64:  [[PTR:%.*]] = bitcast %swift.type* [[TYPE]] to i64*
// CHECK-64:  [[FIELDOFFSETS:%.*]] = getelementptr inbounds i64, i64* [[PTR]], i64 4
// CHECK-64:  [[FIELDOFFSET:%.*]] = getelementptr inbounds i64, i64* [[FIELDOFFSETS]], i32 2
// CHECK-64:  [[OFFSET:%.*]] = load i64, i64* [[FIELDOFFSET]]
// CHECK-64:  [[ADDROFOPT:%.*]] = getelementptr inbounds i8, i8* {{.*}}, i64 [[OFFSET]]
// CHECK-64:  [[OPTPTR:%.*]] = bitcast i8* [[ADDROFOPT]] to %TSq*
// CHECK-64:  call %TSq* @"$S15generic_structsytWb3_"(%TSq* {{.*}}, %TSq* [[OPTPTR]]
