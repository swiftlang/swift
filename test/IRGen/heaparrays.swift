// RUN: %swift -triple x86_64-apple-darwin10 %s -emit-llvm | FileCheck %s

// CHECK: [[REFCOUNT:%.*]] = type { [[TYPE:%swift.type]]*, i64 }
// CHECK: [[INT:%Si]] = type { i64 }
// CHECK: [[OPAQUE:%swift.opaque]] = type opaque

func make_array<T>(n : Int) -> T[] {
  return new T[n]
}

// CHECK:    define { i8*, i64, [[REFCOUNT]]* } @_T10heaparrays10make_arrayU__FT1nSi_GVSs5SliceQ__(

//   Pull out the value witness tables for T.
// CHECK:      [[T0:%.*]] = bitcast [[TYPE]]* %T to i8***
// CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds i8*** [[T0]], i64 -1
// CHECK-NEXT: %T.value = load i8*** [[T1]], align 8

// CHECK:      [[BOUND:%.*]] = call i64 @_TSi18getArrayBoundValuefRSiFT_Bi64_([[INT]]*

//   Grab alignof(T) and use it to compute the alignment.
// CHECK:      [[T0:%.*]] = getelementptr inbounds i8** %T.value, i32 13
// CHECK-NEXT: [[T1:%.*]] = load i8** [[T0]], align 8
// CHECK-NEXT: [[T_ALIGN:%.*]] = ptrtoint i8* [[T1]] to i64
// CHECK-NEXT: [[T0:%.*]] = sub i64 [[T_ALIGN]], 1
// CHECK-NEXT: [[T1:%.*]] = add i64 32, [[T0]]
// CHECK-NEXT: [[T2:%.*]] = xor i64 [[T0]], -1
// CHECK-NEXT: [[HEADER_SIZE:%.*]] = and i64 [[T1]], [[T2]]

//   Compute the required alignment.
// CHECK-NEXT: [[T0:%.*]] = icmp ugt i64 [[T_ALIGN]], 8
// CHECK-NEXT: [[ALLOC_ALIGN:%.*]] = select i1 [[T0]], i64 [[T_ALIGN]], i64 8

//   Grab the stride, compute the allocation size, and allocate.
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds i8** %T.value, i32 14
// CHECK-NEXT: [[T1:%.*]] = load i8** [[T0]], align 8
// CHECK-NEXT: [[T_STRIDE:%.*]] = ptrtoint i8* [[T1]] to i64
// CHECK-NEXT: [[T0:%.*]] = call { i64, i1 } @llvm.umul.with.overflow.i64(i64 [[BOUND]], i64 [[T_STRIDE]])
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
//   FIXME: this should be storing [[BOUND]]!
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[REFCOUNT]]* [[ALLOC]], i32 1
// CHECK-NEXT: [[T1:%.*]] = bitcast [[REFCOUNT]]* [[T0]] to i64*
// CHECK-NEXT: store i64 0, i64* [[T1]], align 8

//   Zero-initialize the elements.
// CHECK-NEXT: [[T0:%.*]] = bitcast [[REFCOUNT]]* [[ALLOC]] to i8*
// CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds i8* [[T0]], i64 [[HEADER_SIZE]]
// CHECK-NEXT: [[T2:%.*]] = bitcast i8* [[T1]] to [[OPAQUE]]*
// CHECK-NEXT: [[T3:%.*]] = sub i64 [[ALLOC_SIZE]], [[HEADER_SIZE]]
// CHECK-NEXT: [[T4:%.*]] = bitcast [[OPAQUE]]* [[T2]] to i8*
// CHECK-NEXT: call void @llvm.memset.p0i8.i64(i8* [[T4]], i8 0, i64 [[T3]], i32 8, i1 false)

//   Create the slice.
// CHECK-NEXT: [[T5:%.*]] = bitcast [[OPAQUE]]* [[T2]] to i8*
// CHECK-NEXT: call { i8*, i64, %swift.refcounted* } @_TVSs5Slice20convertFromHeapArrayU__fMGS_Q__FT4baseBp5ownerBo6lengthBi64__GS_Q__(i8* [[T5]], [[REFCOUNT]]* [[ALLOC]], i64 [[BOUND]], [[TYPE]]* %T)

// CHECK:    define internal i64 @arraydestroy([[REFCOUNT]]*
//   Load the binding.
// CHECK:      [[T0:%.*]] = bitcast [[REFCOUNT]]* [[ALLOC:%.*]] to i8*
// CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds i8* [[T0]], i32 24
// CHECK-NEXT: [[T2:%.*]] = bitcast i8* [[T1]] to [[TYPE]]**
// CHECK-NEXT: %T = load [[TYPE]]** [[T2]], align 8
// CHECK-NEXT: [[T0:%.*]] = bitcast [[TYPE]]* %T to i8***
// CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds i8*** [[T0]], i64 -1
// CHECK-NEXT: %T.value = load i8*** [[T1]], align 8
//   Compute the header size.
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds i8** %T.value, i32 13
// CHECK-NEXT: [[T1:%.*]] = load i8** [[T0]], align 8
// CHECK-NEXT: [[T_ALIGN:%.*]] = ptrtoint i8* [[T1]] to i64
// CHECK-NEXT: [[T0:%.*]] = sub i64 [[T_ALIGN]], 1
// CHECK-NEXT: [[T1:%.*]] = add i64 32, [[T0]]
// CHECK-NEXT: [[T2:%.*]] = xor i64 [[T0]], -1
// CHECK-NEXT: [[HEADER_SIZE:%.*]] = and i64 [[T1]], [[T2]]
//   Load the length.
// CHECK:      [[T0:%.*]] = getelementptr inbounds [[REFCOUNT]]* [[ALLOC]], i32 1
// CHECK-NEXT: [[T1:%.*]] = bitcast [[REFCOUNT]]* [[T0]] to i64*
// CHECK-NEXT: [[BOUND:%.*]] = load i64* [[T1]], align 8
//   Load the stride and find the limits of the array.
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds i8** %T.value, i32 14
// CHECK-NEXT: [[T1:%.*]] = load i8** [[T0]], align 8
// CHECK-NEXT: [[T_STRIDE:%.*]] = ptrtoint i8* [[T1]] to i64
// CHECK-NEXT: [[T0:%.*]] = bitcast [[REFCOUNT]]* [[ALLOC:%.*]] to i8*
// CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds i8* [[T0]], i64 [[HEADER_SIZE]]
// CHECK-NEXT: [[BEGIN:%.*]] = bitcast i8* [[T1]] to [[OPAQUE]]*
// CHECK-NEXT: [[T0:%.*]] = bitcast [[OPAQUE]]* [[BEGIN]] to i8*
// CHECK-NEXT: [[T1:%.*]] = mul i64 [[T_STRIDE]], [[BOUND]]
// CHECK-NEXT: [[T2:%.*]] = getelementptr inbounds i8* [[T0]], i64 [[T1]]
// CHECK-NEXT: [[END:%.*]] = bitcast i8* [[T2]] to [[OPAQUE]]*
//   Loop over the elements.
// CHECK-NEXT: icmp eq [[OPAQUE]]* [[BEGIN]], [[END]]
// CHECK-NEXT: br i1
