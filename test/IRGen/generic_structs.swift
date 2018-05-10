// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -primary-file %s -emit-ir | %FileCheck %s --check-prefix=CHECK --check-prefix=CHECK-%target-ptrsize -DINT=i%target-ptrsize -DINT_32=i32

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

// CHECK-LABEL: define{{.*}} swiftcc void @"$S15generic_structs13GenericStructVACyxGycfC"
// CHECK:  [[T0:%.*]] = call swiftcc %swift.metadata_response @"$S15generic_structs13GenericStructVMa"([[INT]] 0, %swift.type* %T, i8** %T.Proto)
// CHECK:  [[TYPE:%.*]] = extractvalue %swift.metadata_response [[T0]], 0
// CHECK:  [[PTR:%.*]] = bitcast %swift.type* [[TYPE]] to [[INT_32]]*
// CHECK:  [[FIELDOFFSETS:%.*]] = getelementptr inbounds [[INT_32]], [[INT_32]]* [[PTR]], [[INT]] [[IDX:4|8]]
// CHECK:  [[FIELDOFFSET:%.*]] = getelementptr inbounds [[INT_32]], [[INT_32]]* [[FIELDOFFSETS]], i32 2
// CHECK:  [[OFFSET:%.*]] = load [[INT_32]], [[INT_32]]* [[FIELDOFFSET]]
// CHECK:  [[ADDROFOPT:%.*]] = getelementptr inbounds i8, i8* {{.*}}, [[INT_32]] [[OFFSET]]
// CHECK:  [[OPTPTR:%.*]] = bitcast i8* [[ADDROFOPT]] to %TSq*
// CHECK:  call %TSq* @"$SxSg15generic_structs5ProtoRzlWOb"(%TSq* {{.*}}, %TSq* [[OPTPTR]]
