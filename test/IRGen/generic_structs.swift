// RUN: %target-swift-frontend -primary-file %s -emit-ir | %FileCheck %s --check-prefix=CHECK --check-prefix=CHECK-%target-ptrsize -DINT=i%target-ptrsize -DINT_32=i32

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

// CHECK-LABEL: define{{.*}} swiftcc void @"$s15generic_structs13GenericStructVACyxGycfC"
// CHECK: [[VTABLE:%.*]] = bitcast i8** %.valueWitnesses to %swift.vwtable*
// CHECK-NEXT: [[SIZE_PTR:%.*]] = getelementptr inbounds %swift.vwtable, %swift.vwtable* [[VTABLE]]
// CHECK-NEXT: [[SIZE:%.*]] = load i64, i64* [[SIZE_PTR]]
// CHECK-NEXT: [[EMPTY_PTR:%.*]] = alloca i8, i64 [[SIZE]]
// CHECK-NEXT: call void @llvm.lifetime.start.p0i8({{.*}} [[EMPTY_PTR]])
// CHECK-NEXT: [[EMPTY:%.*]] = bitcast i8* [[EMPTY_PTR]]  to %TSq*

// CHECK: [[SELF_PTR:%.*]] = bitcast i8* %self to %T15generic_structs13GenericStructV*

// CHECK: [[SELF_RAW_PTR:%.*]] = bitcast %T15generic_structs13GenericStructV* [[SELF_PTR]] to i8*
// CHECK-NEXT: [[OPT_PTR:%.*]] = getelementptr inbounds i8, i8* [[SELF_RAW_PTR]]
// CHECK-NEXT: [[OPTIONAL:%.*]] = bitcast i8* [[OPT_PTR]] to %TSq*
// CHECK-NEXT: [[METADATA:%.*]] = call swiftcc %swift.metadata_response @"$sSqMa"(i64 0, %swift.type* %T)
// CHECK-NEXT: [[META_VAL:%.*]] = extractvalue %swift.metadata_response [[METADATA]], 0
// CHECK: %26 = call %TSq* @"$sxSg15generic_structs5ProtoRzlWOb"(%TSq* [[EMPTY]], %TSq* [[OPTIONAL]]
// CHECK: %27 = call %T15generic_structs13GenericStructV* @"$s15generic_structs13GenericStructVyxGAA5ProtoRzlWOb"(%T15generic_structs13GenericStructV* [[SELF_PTR]], %T15generic_structs13GenericStructV* %0
