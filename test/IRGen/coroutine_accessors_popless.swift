// RUN: %target-swift-emit-irgen                                            \
// RUN:     %s                                                              \
// RUN:     -Onone                                                          \
// RUN:     -enable-callee-allocated-coro-abi                               \
// RUN:     -enable-experimental-feature CoroutineAccessors                 \
// RUN:     -enable-arm64-corocc                                            \
// RUN:     -enable-x86_64-corocc                                           \
// RUN: | %IRGenFileCheck %s

// REQUIRES: CPU=arm64 || CPU=arm64e || CPU=x86_64
// REQUIRES: swift_feature_CoroutineAccessors

// CHECK-LABEL: @"$s27coroutine_accessors_popless1iSivyTwc" = {{.*}}global{{.*}} %swift.coro_func_pointer <{
//           :    sub (
// CHECK-SAME:      $s27coroutine_accessors_popless1iSivy
//           :      $s27coroutine_accessors_popless1iSivyTwc
//           :    ),
// CHECK-SAME:    i32 0
// CHECK-SAME:  }>
// CHECK-LABEL: @"$s27coroutine_accessors_popless1iSivxTwc" = {{.*}}global{{.*}} %swift.coro_func_pointer <{
//           :    sub (
// CHECK-SAME:      $s27coroutine_accessors_popless1iSivx
//           :      $s27coroutine_accessors_popless1iSivxTwc
//           :    ),
// CHECK-SAME:    i32 0
// CHECK-SAME:  }>

// CHECK-arm64e-LABEL: _swift_coro_task_alloc.ptrauth = private constant {
// CHECK-arm64e-SAME:    ptr @_swift_coro_task_alloc,
// CHECK-arm64e-SAME:    i32 0,
// CHECK-arm64e-SAME:    i64 ptrtoint (
// CHECK-arm64e-SAME:      ptr getelementptr inbounds (
// CHECK-arm64e-SAME:        ptr @_swift_coro_async_allocator,
// CHECK-arm64e-SAME:        i32 0,
// CHECK-arm64e-SAME:        i32 1
// CHECK-arm64e-SAME:      )
// CHECK-arm64e-SAME:    )
// CHECK-arm64e-SAME:    i64 24469 }
// CHECK-arm64e-SAME:  section "llvm.ptrauth"
// CHECK-arm64e-SAME:  align 8
// CHECK-arm64e-LABEL: @_swift_coro_task_dealloc.ptrauth = private constant {
// CHECK-arm64e-SAME:    ptr @_swift_coro_task_dealloc,
// CHECK-arm64e-SAME:    i32 0,
// CHECK-arm64e-SAME:    i64 ptrtoint (
// CHECK-arm64e-SAME:      ptr getelementptr inbounds (
// CHECK-arm64e-SAME:        ptr @_swift_coro_async_allocator,
// CHECK-arm64e-SAME:        i32 0,
// CHECK-arm64e-SAME:        i32 2
// CHECK-arm64e-SAME:      )
// CHECK-arm64e-SAME:    )
// CHECK-arm64e-SAME:    i64 40879 },
// CHECK-arm64e-SAME:  section "llvm.ptrauth",
// CHECK-arm64e-SAME:  align 8
// CHECK-LABEL: _swift_coro_async_allocator = linkonce_odr hidden constant %swift.coro_allocator {
// CHECK-SAME:      i32 1,
// CHECK-SAME:      _swift_coro_task_alloc
// CHECK-SAME:      _swift_coro_task_dealloc
// CHECK-SAME:  }
// CHECK-arm64e-LABEL: _swift_coro_typed_malloc.ptrauth = private constant {
// CHECK-arm64e-SAME:    ptr @_swift_coro_typed_malloc,
// CHECK-arm64e-SAME:    i32 0,
// CHECK-arm64e-SAME:    i64 ptrtoint (
// CHECK-arm64e-SAME:      ptr getelementptr inbounds (
// CHECK-arm64e-SAME:        %swift.coro_allocator,
// CHECK-arm64e-SAME:        ptr @_swift_coro_typed_malloc_allocator,
// CHECK-arm64e-SAME:        i32 0,
// CHECK-arm64e-SAME:        i32 1
// CHECK-arm64e-SAME:      ) to i64
// CHECK-arm64e-SAME:    ),
// CHECK-arm64e-SAME:    i64 24469 }
// CHECK-arm64e-SAME:  section "llvm.ptrauth"
// CHECK-arm64e-SAME:  align 8
// CHECK-arm64e-LABEL: _swift_coro_free.ptrauth = private constant {
// CHECK-arm64e-SAME:    ptr @_swift_coro_free,
// CHECK-arm64e-SAME:    i32 0,
// CHECK-arm64e-SAME:    i64 ptrtoint (
// CHECK-arm64e-SAME:      ptr getelementptr inbounds (
// CHECK-arm64e-SAME:        %swift.coro_allocator,
// CHECK-arm64e-SAME:        ptr @_swift_coro_typed_malloc_allocator,
// CHECK-arm64e-SAME:        i32 0,
// CHECK-arm64e-SAME:        i32 2
// CHECK-arm64e-SAME:      ) to i64
// CHECK-arm64e-SAME:    ),
// CHECK-arm64e-SAME:    i64 40879 },
// CHECK-arm64e-SAME:  section "llvm.ptrauth",
// CHECK-arm64e-SAME:  align 8
// CHECK-apple-LABEL: _swift_coro_typed_malloc_allocator = linkonce_odr hidden constant %swift.coro_allocator {
// CHECK-apple-SAME:       i32 259,
// CHECK-apple-SAME:       _swift_coro_typed_malloc
// CHECK-apple-SAME:       _swift_coro_free 
// CHECK-apple-SAME:  }

// CHECK-LABEL: @_swift_coro_alloc(
// CHECK-SAME:      ptr [[FRAME:%[^,]+]]
// CHECK-SAME:      ptr swiftcoro [[ALLOCATOR:%[^,]+]],
// CHECK-SAME:      i64 [[SIZE:%[^,]+]]
// CHECK-SAME:      i64 [[TYPE_ID:%[^)]+]]
// CHECK-SAME:  )
// CHECK-SAME:  {
// CHECK:       entry:
// CHECK:         [[USE_POPLESS:%[^,]+]] = icmp eq ptr [[ALLOCATOR]], null
// CHECK:         br i1 [[USE_POPLESS]],
// CHECK-SAME:        label %popless
// CHECK-SAME:        label %normal
// CHECK:       popless:
// CHECK:         [[STACK_ALLOCATION:%[^,]+]] = alloca i8, i64 [[SIZE]]
// CHECK:         musttail call void @llvm.ret.popless()
// CHECK:         ret ptr [[STACK_ALLOCATION]]
// CHECK:       normal:
// CHECK:         [[ALLOCATE_FN_PTR:%[^,]+]] = getelementptr inbounds %swift.coro_allocator
// CHECK-SAME:        ptr [[ALLOCATOR]]
// CHECK-SAME:        i32 0
// CHECK-SAME:        i32 1
// CHECK:         [[ALLOCATE_FN:%[^,]+]] = load ptr, ptr [[ALLOCATE_FN_PTR]]
// CHECK-arm64e:  [[ALLOCATE_FN_PTR_BITS:%[^,]+]] = ptrtoint ptr [[ALLOCATE_FN_PTR]] to i64
// CHECK-arm64e:  [[ALLOCATE_FN_DISCRIMINATOR:%[^,]+]] = call i64 @llvm.ptrauth.blend(i64 [[ALLOCATE_FN_PTR_BITS]], i64 24469)
// CHECK-arm64e:  [[ALLOCATE_FN_BITS:%[^,]+]] = ptrtoint ptr [[ALLOCATE_FN]] to i64
// CHECK-arm64e:  [[ALLOCATE_FN_BITS_AUTHED:%[^,]+]] = call i64 @llvm.ptrauth.auth(i64 [[ALLOCATE_FN_BITS]], i32 0, i64 [[ALLOCATE_FN_DISCRIMINATOR]])
// CHECK-arm64e:  [[ALLOCATE_FN:%[^,]+]] = inttoptr i64 [[ALLOCATE_FN_BITS_AUTHED]]
// CHECK:         [[ALLOCATION:%[^,]+]] = call swiftcc ptr [[ALLOCATE_FN]](
// CHECK-SAME:        ptr [[FRAME]],
// CHECK-SAME:        ptr swiftcoro [[ALLOCATOR]],
// CHECK-SAME:        [[INT]] [[SIZE]],
// CHECK-SAME:        [[INT]] [[TYPE_ID]]
// CHECK-SAME:    )
// CHECK:         ret ptr [[ALLOCATION]]
// CHECK:       }

// CHECK-LABEL: @_swift_coro_dealloc(
// CHECK-SAME:      ptr [[FRAME:%[^,]+]]
// CHECK-SAME:      ptr swiftcoro [[ALLOCATOR:%[^,]+]],
// CHECK-SAME:      ptr [[ADDRESS:%[^)]+]]
// CHECK-SAME:  )
// CHECK-SAME:  {
// CHECK:       entry:
// CHECK:         [[BAIL:%[^,]+]] = icmp eq ptr [[ALLOCATOR]], null
// CHECK:         br i1 [[BAIL]],
// CHECK-SAME:        label %null_allocator
// CHECK-SAME:        label %nonnull_allocator
// CHECK:       null_allocator:
// CHECK:         ret void
// CHECK:       nonnull_allocator:
// CHECK:         [[FLAGS_ADDR:%[^,]+]] = getelementptr inbounds %swift.coro_allocator
// CHECK-SAME:        ptr [[ALLOCATOR]]
// CHECK-SAME:        i32 0
// CHECK-SAME:        i32 0
// CHECK:         [[FLAGS:%[^,]+]] = load i32, ptr [[FLAGS_ADDR]], align 4
// CHECK:         [[DEALLOC_DEFERRING_ALLOCATOR:%[^,]+]] = and i32 [[FLAGS]], 256
// CHECK:         [[IS_DEALLOC_DEFERRING_ALLOCATOR:%[^,]+]] = icmp ne i32 [[DEALLOC_DEFERRING_ALLOCATOR]], 0
// CHECK:         br i1 [[IS_DEALLOC_DEFERRING_ALLOCATOR]]
// CHECK-SAME:        label %deferring_allocator
// CHECK-SAME:        label %normal
// CHECK:         deferring_allocator:
// CHECK:           ret void
// CHECK:         normal:
// CHECK:           [[DEALLOCATE_FN_PTR:%[^,]+]] = getelementptr inbounds %swift.coro_allocator
// CHECK-SAME:          ptr [[ALLOCATOR]]
// CHECK-SAME:          i32 0
// CHECK-SAME:          i32 2
// CHECK:           [[DEALLOCATE_FN:%[^,]+]] = load ptr, ptr [[DEALLOCATE_FN_PTR]]
// CHECK-arm64e:    [[DEALLOCATE_FN_PTR_BITS:%[^,]+]] = ptrtoint ptr [[DEALLOCATE_FN_PTR]] to i64
// CHECK-arm64e:    [[DEALLOCATE_FN_DISCRIMINATOR:%[^,]+]] = call i64 @llvm.ptrauth.blend(i64 [[DEALLOCATE_FN_PTR_BITS]], i64 40879)
// CHECK-arm64e:    [[DEALLOCATE_FN_BITS:%[^,]+]] = ptrtoint ptr [[DEALLOCATE_FN]] to i64
// CHECK-arm64e:    [[DEALLOCATE_FN_BITS_AUTHED:%[^,]+]] = call i64 @llvm.ptrauth.auth(i64 [[DEALLOCATE_FN_BITS]], i32 0, i64 [[DEALLOCATE_FN_DISCRIMINATOR]])
// CHECK-arm64e:    [[DEALLOCATE_FN:%[^,]+]] = inttoptr i64 [[DEALLOCATE_FN_BITS_AUTHED]]
// CHECK:           call swiftcc void [[DEALLOCATE_FN]](ptr [[FRAME]], ptr swiftcoro [[ALLOCATOR]], ptr [[ADDRESS]])
// CHECK:         ret void
// CHECK:       }

public var _i: Int = 0

public var i: Int {
  yielding borrow {
    yield _i
  }
  yielding mutate {
    yield &_i
  }
}

// CHECK-LABEL: define{{.*}} void @increment_i(
// CHECK-SAME:  )
// CHECK-SAME:  {
//      :         [[SIZE_32:%[^,]+]] = load i32
//           :        ptr getelementptr inbounds (
//           :            %swift.coro_func_pointer
// CHECK:                 $s27coroutine_accessors_popless1iSivxTwc
//           :            i32 0
//           :            i32 1
//           :        )
// CHECK-64:      [[SIZE_64:%[^,]+]] = zext i32 {{%[^,]+}} to i64
// CHECK-64:      [[FRAME:%[^,]+]] = alloca i8, [[INT]] [[SIZE_64]]
// CHECK:         call void @llvm.lifetime.start.p0(i64 -1, ptr [[FRAME]])
// CHECK:         [[RAMP:%[^,]+]] = call ptr @llvm.coro.prepare.retcon(ptr @"$s27coroutine_accessors_popless1iSivx")
// CHECK:         [[RETVAL:%[^,]+]] = call swiftcorocc { ptr, ptr } [[RAMP]](
// CHECK-SAME:         [[FRAME]],
// CHECK-SAME:         null
// CHECK-SAME:    )
// CHECK:         [[CONTINUATION:%[^,]+]] = extractvalue { ptr, ptr } [[RETVAL]], 0
// CHECK:         [[YIELD:%[^,]+]] = extractvalue { ptr, ptr } [[RETVAL]], 1
// CHECK:         call swiftcc void @"$s27coroutine_accessors_popless9incrementyySizF"(
// CHECK-SAME:        [[YIELD]]
// CHECK-SAME:    )
// CHECK:         call swiftcorocc void [[CONTINUATION]](
// CHECK-SAME:        [[FRAME]],
// CHECK-SAME:        null
// CHECK-SAME:    )
// CHECK:         call void @llvm.lifetime.end.p0(i64 -1, ptr [[FRAME]])
// CHECK:       }
@_silgen_name("increment_i")
public func increment_i() {
  increment(&i)
}

public func increment(_ int: inout Int) {
  int += 1
}

// CHECK-LABEL: define{{.*}} void @increment_i_async(
//                  ptr swiftasync %0
// CHECK-SAME:  )
// CHECK-SAME:  {
//      :         [[SIZE_32:%[^,]+]] = load i32
//           :        ptr getelementptr inbounds (
//           :            %swift.coro_func_pointer
// CHECK:                 $s27coroutine_accessors_popless1iSivxTwc
//           :            i32 0
//           :            i32 1
//           :        )
// CHECK:         [[SIZE_RAW:%[^,]+]] = zext i32 {{%[^,]+}} to i64
// CHECK:         [[SIZE_1:%[^,]+]] = add i64 [[SIZE_RAW]], 15
// CHECK:         [[SIZE:%[^,]+]] = and i64 [[SIZE_1]], -16
// CHECK:         [[FRAME:%[^,]+]] = call swiftcc ptr @swift_task_alloc(i64 [[SIZE]])
// CHECK:         call void @llvm.lifetime.start.p0(i64 -1, ptr [[FRAME]])
// CHECK:         [[RAMP:%[^,]+]] = call ptr @llvm.coro.prepare.retcon(ptr @"$s27coroutine_accessors_popless1iSivx")
// CHECK:         [[RETVAL:%[^,]+]] = call swiftcorocc { ptr, ptr } [[RAMP]](
// CHECK-SAME:         [[FRAME]],
// CHECK-SAME:         _swift_coro_async_allocator
// CHECK-SAME:    )
// CHECK:         [[CONTINUATION:%[^,]+]] = extractvalue { ptr, ptr } [[RETVAL]], 0
// CHECK:         [[YIELD:%[^,]+]] = extractvalue { ptr, ptr } [[RETVAL]], 1

//                increment_async([[YIELD]])

// CHECK:         call swiftcorocc void [[CONTINUATION]](
// CHECK-SAME:        [[FRAME]],
// CHECK-SAME:        _swift_coro_async_allocator
// CHECK-SAME:    )
// CHECK:         call void @llvm.lifetime.end.p0(i64 -1, ptr [[FRAME]])
// CHECK:         call swiftcc void @{{(_)?}}swift_task_dealloc_through(ptr [[FRAME]])
// CHECK:       }
@_silgen_name("increment_i_async")
public func increment_i_async() async {
  await increment_async(&i)
}

public func increment_async(_ int: inout Int) async {
  int += 1
}

public var force_yield_once_convention : () {
  _read {
    let nothing: () = ()
    yield nothing
  }
// CHECK-LABEL: define{{.*}} { ptr, ptr } @increment_i_yield_once(
//                  ptr noalias dereferenceable(32) %0
// CHECK-SAME:  )
// CHECK-SAME:  {
//      :         [[SIZE_32:%[^,]+]] = load i32
//           :        ptr getelementptr inbounds (
//           :            %swift.coro_func_pointer
// CHECK:                 $s27coroutine_accessors_popless1iSivxTwc
//           :            i32 0
//           :            i32 1
//           :        )
// CHECK:         [[TYPE_ID:%[^,]+]] = load i64
// CHECK:         [[SIZE:%[^,]+]] = zext i32 {{%[^,]+}} to i64
// CHECK:         [[ALLOCATION:%[^,]+]] = call token{{.*}} @llvm.coro.alloca.alloc.frame.i64(i64 [[SIZE]], i32 16, i64 [[TYPE_ID]])
// CHECK:         [[FRAME:%[^,]+]] = call ptr @llvm.coro.alloca.get(token [[ALLOCATION]])
// CHECK:         call void @llvm.lifetime.start.p0(i64 -1, ptr [[FRAME]])
// CHECK:         [[RAMP:%[^,]+]] = call ptr @llvm.coro.prepare.retcon(ptr @"$s27coroutine_accessors_popless1iSivx")
// CHECK:         [[RETVAL:%[^,]+]] = call swiftcorocc { ptr, ptr } [[RAMP]](
// CHECK-SAME:         [[FRAME]],
// CHECK-apple-SAME:         _swift_coro_typed_malloc_allocator
// CHECK-SAME:    )
// CHECK:         [[CONTINUATION:%[^,]+]] = extractvalue { ptr, ptr } [[RETVAL]], 0
// CHECK:         [[YIELD:%[^,]+]] = extractvalue { ptr, ptr } [[RETVAL]], 1
// CHECK:         call swiftcc void @"$s27coroutine_accessors_popless9incrementyySizF"(
// CHECK-SAME:        [[YIELD]]
// CHECK-SAME:    )
// CHECK:         call swiftcorocc void [[CONTINUATION]](
// CHECK-SAME:        [[FRAME]],
// CHECK-apple-SAME:        _swift_coro_typed_malloc_allocator
// CHECK-SAME:    )
// CHECK:         call void @llvm.lifetime.end.p0(i64 -1, ptr [[FRAME]])
// CHECK:         call void @llvm.coro.alloca.free.frame(token [[ALLOCATION]])
// CHECK:       }
  @_silgen_name("increment_i_yield_once")
  _modify {
    increment(&i)

    var nothing: () = ()
    yield &nothing
  }
}

public var force_yield_once_2_convention : () {
  yielding borrow {
    let nothing: () = ()
    yield nothing
  }
// CHECK-LABEL: define{{.*}} { ptr, ptr } @increment_i_yield_once_2(
//                  ptr noalias %0
// CHECK-SAME:      ptr swiftcoro [[ALLOCATOR:%[^)]+]]
// CHECK-SAME:  )
// CHECK-SAME:  {
//      :         [[SIZE_32:%[^,]+]] = load i32
//           :        ptr getelementptr inbounds (
//           :            %swift.coro_func_pointer
// CHECK:                 $s27coroutine_accessors_popless1iSivxTwc
//           :            i32 0
//           :            i32 1
//           :        )
// CHECK:         [[TYPE_ID:%[^,]+]] = load i64
// CHECK:         [[SIZE:%[^,]+]] = zext i32 {{%[^,]+}} to i64
// CHECK:         [[ALLOCATION:%[^,]+]] = call token{{.*}} @llvm.coro.alloca.alloc.frame.i64(i64 [[SIZE]], i32 16, i64 [[TYPE_ID]])
// CHECK:         [[FRAME:%[^,]+]] = call ptr @llvm.coro.alloca.get(token [[ALLOCATION]])
// CHECK:         call void @llvm.lifetime.start.p0(i64 -1, ptr [[FRAME]])
// CHECK:         [[RAMP:%[^,]+]] = call ptr @llvm.coro.prepare.retcon(ptr @"$s27coroutine_accessors_popless1iSivx")
// CHECK:         [[RETVAL:%[^,]+]] = call swiftcorocc { ptr, ptr } [[RAMP]](
// CHECK-SAME:         [[FRAME]],
// CHECK-SAME:         [[ALLOCATOR]]
// CHECK-SAME:    )
// CHECK:         [[CONTINUATION:%[^,]+]] = extractvalue { ptr, ptr } [[RETVAL]], 0
// CHECK:         [[YIELD:%[^,]+]] = extractvalue { ptr, ptr } [[RETVAL]], 1
// CHECK:         call swiftcc void @"$s27coroutine_accessors_popless9incrementyySizF"(
// CHECK-SAME:        [[YIELD]]
// CHECK-SAME:    )
// CHECK:         call swiftcorocc void [[CONTINUATION]](
// CHECK-SAME:        [[FRAME]],
// CHECK-SAME:        [[ALLOCATOR]]
// CHECK-SAME:    )
// CHECK:         call void @llvm.lifetime.end.p0(i64 -1, ptr [[FRAME]])
// CHECK:         call void @llvm.coro.alloca.free.frame(token [[ALLOCATION]])
// CHECK:       }
  @_silgen_name("increment_i_yield_once_2")
  yielding mutate {
    increment(&i)

    var nothing: () = ()
    yield &nothing
  }
}
