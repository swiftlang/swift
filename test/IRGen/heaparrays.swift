// RUN: %swift -target x86_64-apple-darwin10 %s -emit-ir | FileCheck %s

// CHECK: [[REFCOUNT:%.*]] = type { [[TYPE:%swift.type]]*, i32, i32 }
// CHECK: [[OPAQUE:%swift.opaque]] = type opaque

func make_array<T>(var n: Int, var x: T) -> T[] {
  return new T[n] {i in x}
}

// CHECK: define i64 @_TF10heaparrays10make_array{{.*}}(i64, %swift.opaque* noalias, %swift.type* %T)

// CHECK: call [[OPAQUE]]* %allocateBuffer(
// CHECK: call [[OPAQUE]]* %initializeWithTake(

//   Grab alignof(T) and use it to compute the alignment.
// CHECK:      [[T0:%.*]] = bitcast [[TYPE]]* %T to i8***
// CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds i8*** [[T0]], i64 -1
// CHECK-NEXT: [[T_VALUE:%.*]] = load i8*** [[T1]], align 8
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds i8** [[T_VALUE]], i32 18
// CHECK-NEXT: [[T1:%.*]] = load i8** [[T0]], align 8
// CHECK-NEXT: [[T2:%.*]] = ptrtoint i8* [[T1]] to i64
// CHECK-NEXT: [[T_ALIGNMASK:%.*]] = and i64 [[T2]], 65535
// CHECK-NEXT: [[T0:%.*]] = add i64 32, [[T_ALIGNMASK]]
// CHECK-NEXT: [[T1:%.*]] = xor i64 [[T_ALIGNMASK]], -1
// CHECK-NEXT: [[HEADER_SIZE:%.*]] = and i64 [[T0]], [[T1]]

//   Compute the required alignment.
// CHECK-NEXT: [[T0:%.*]] = icmp ugt i64 [[T_ALIGNMASK]], 7
// CHECK-NEXT: [[ALLOC_ALIGNMASK:%.*]] = select i1 [[T0]], i64 [[T_ALIGNMASK]], i64 7

//   Grab the stride, compute the allocation size, and allocate.
// CHECK-NEXT: [[T0:%.*]] = bitcast [[TYPE]]* %T to i8***
// CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds i8*** [[T0]], i64 -1
// CHECK-NEXT: [[T_VALUE:%.*]] = load i8*** [[T1]], align 8
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds i8** [[T_VALUE]], i32 19
// CHECK-NEXT: [[T1:%.*]] = load i8** [[T0]], align 8
// CHECK-NEXT: [[T_STRIDE:%.*]] = ptrtoint i8* [[T1]] to i64
// CHECK-NEXT: [[T0:%.*]] = call { i64, i1 } @llvm.umul.with.overflow.i64(i64 [[BOUND:%0]], i64 [[T_STRIDE]])
// CHECK-NEXT: [[T1:%.*]] = extractvalue { i64, i1 } [[T0]], 0
// CHECK-NEXT: [[T2:%.*]] = extractvalue { i64, i1 } [[T0]], 1
// CHECK-NEXT: [[T3:%.*]] = select i1 [[T2]], i64 -1, i64 [[T1]]
// CHECK-NEXT: [[T0:%.*]] = call { i64, i1 } @llvm.uadd.with.overflow.i64(i64 [[T3]], i64 [[HEADER_SIZE]])
// CHECK-NEXT: [[T1:%.*]] = extractvalue { i64, i1 } [[T0]], 0
// CHECK-NEXT: [[T2:%.*]] = extractvalue { i64, i1 } [[T0]], 1
// CHECK-NEXT: [[ALLOC_SIZE:%.*]] = select i1 [[T2]], i64 -1, i64 [[T1]]
// CHECK-NEXT: [[ALLOC:%.*]] = call noalias [[REFCOUNT]]* @swift_allocObject({{.*}}

//   Initialize the binding.
// CHECK-NEXT: [[T0:%.*]] = bitcast [[REFCOUNT]]* [[ALLOC]] to i8*
// CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds i8* [[T0]], i32 24
// CHECK-NEXT: [[T2:%.*]] = bitcast i8* [[T1]] to [[TYPE]]**
// CHECK-NEXT: store [[TYPE]]* %T, [[TYPE]]** [[T2]], align 8

//   Initialize the length.
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[REFCOUNT]]* [[ALLOC]], i32 1
// CHECK-NEXT: [[T1:%.*]] = bitcast [[REFCOUNT]]* [[T0]] to i64*
// CHECK-NEXT: store i64 [[BOUND]], i64* [[T1]], align 8

//   Zero-initialize the elements.
// CHECK-NEXT: [[T0:%.*]] = bitcast [[REFCOUNT]]* [[ALLOC]] to i8*
// CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds i8* [[T0]], i64 [[HEADER_SIZE]]
// CHECK-NEXT: [[T2:%.*]] = bitcast i8* [[T1]] to [[OPAQUE]]*
// CHECK-NEXT: [[T3:%.*]] = sub i64 [[ALLOC_SIZE]], [[HEADER_SIZE]]
// CHECK-NEXT: [[T4:%.*]] = bitcast [[OPAQUE]]* [[T2]] to i8*
// CHECK-NEXT: call void @llvm.memset.p0i8.i64(i8* [[T4]], i8 0, i64 [[T3]], i32 8, i1 false)

//   Create the array.
// CHECK-NEXT: [[T5:%.*]] = bitcast [[OPAQUE]]* [[T2]] to i8*
// CHECK-NEXT: call [[arrayLayout:[^@]*]] @_TFSa20convertFromHeapArray{{.*}}(i8* [[T5]], [[REFCOUNT]]* [[ALLOC]], i64 [[BOUND]], [[TYPE]]* %T)

// CHECK:    define internal void @arraydestroy([[REFCOUNT]]*
//   Load the binding.
// CHECK:      [[T0:%.*]] = bitcast [[REFCOUNT]]* [[ALLOC:%.*]] to i8*
// CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds i8* [[T0]], i32 24
// CHECK-NEXT: [[T2:%.*]] = bitcast i8* [[T1]] to [[TYPE]]**
// CHECK-NEXT: %T = load [[TYPE]]** [[T2]], align 8
// CHECK:  store [[TYPE]]*
//   Compute the header size.
// CHECK-NEXT: [[T0:%.*]] = bitcast [[TYPE]]* %T to i8***
// CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds i8*** [[T0]], i64 -1
// CHECK-NEXT: [[T_VALUE:%.*]] = load i8*** [[T1]], align 8
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds i8** [[T_VALUE]], i32 18
// CHECK-NEXT: [[T1:%.*]] = load i8** [[T0]], align 8
// CHECK-NEXT: [[T2:%.*]] = ptrtoint i8* [[T1]] to i64
// CHECK-NEXT: [[T_ALIGNMASK:%.*]] = and i64 [[T2]], 65535
// CHECK-NEXT: [[T0:%.*]] = add i64 32, [[T_ALIGNMASK]]
// CHECK-NEXT: [[T1:%.*]] = xor i64 [[T_ALIGNMASK]], -1
// CHECK-NEXT: [[HEADER_SIZE:%.*]] = and i64 [[T0]], [[T1]]
//   Load the length.
// CHECK:      [[T0:%.*]] = getelementptr inbounds [[REFCOUNT]]* [[ALLOC]], i32 1
// CHECK-NEXT: [[T1:%.*]] = bitcast [[REFCOUNT]]* [[T0]] to i64*
// CHECK-NEXT: [[BOUND:%.*]] = load i64* [[T1]], align 8
//   Load the stride and find the limits of the array.
// CHECK:      [[BEGINP:%.*]] = getelementptr inbounds i8* {{%.*}}, i64 [[HEADER_SIZE]]
// CHECK:      [[BEGIN:%.*]] = bitcast i8* [[BEGINP]] to [[OPAQUE]]*
// CHECK:      %stride = ptrtoint i8* {{%.*}} to i64
// CHECK:      [[DISTANCE:%.*]] = mul nsw i64 [[BOUND]], %stride
// CHECK:      [[ENDP:%.*]] = getelementptr inbounds i8* {{%.*}}, i64 [[DISTANCE]]
// CHECK:      [[END:%.*]] = bitcast i8* [[ENDP]] to [[OPAQUE]]*
// CHECK:      icmp eq [[OPAQUE]]* [[BEGIN]], [[END]]


// CHECK: define [[arrayLayout]] @_TF10heaparrays22make_array_enumerators{{.*}}(i64, %swift.opaque* noalias, %swift.type* [[T:%[0-9a-zA-Z]*]],
func make_array_enumerators<T : Generator>(n: Int, x: T) -> T[] {
  // CHECK: call [[arrayLayout]] @_TFSa20convertFromHeapArray{{.*}}(i8* [[STORAGE:%[0-9a-zA-Z]*]], %swift.refcounted* [[OWNER:%[0-9a-zA-Z]*]], i64 [[LENGTH:%[0-9a-zA-Z]*]], %swift.type* [[T]])
  return new T[n] { i in x }
}
