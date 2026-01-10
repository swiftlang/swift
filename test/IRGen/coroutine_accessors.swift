// RUN: %target-swift-emit-irgen                                            \
// RUN:     %s                                                              \
// RUN:     -enable-callee-allocated-coro-abi                               \
// RUN:     -enable-experimental-feature CoroutineAccessors                 \
// RUN: | %IRGenFileCheck %s

// REQUIRES: swift_feature_CoroutineAccessors

// CHECK-LABEL: %T19coroutine_accessors1SV = type <{ %AnyObject, %TSi }>

// CHECK-LABEL: @"$s19coroutine_accessors1SV3irmSivyTwc" = {{.*}}global %swift.coro_func_pointer
//           :    sub (
// CHECK-SAME:        $s19coroutine_accessors1SV3irmSivy
//           :        $s19coroutine_accessors1SV3irmSivyTwc
//           :    )
// CHECK-SAME:    i32 0
// CHECK-SAME:  }>
// CHECK-LABEL: @"$s19coroutine_accessors1SV3irmSivxTwc" = {{.*}}global{{.*}} %swift.coro_func_pointer <{
//           :    sub (
// CHECK-SAME:      $s19coroutine_accessors1SV3irmSivx
//           :      $s19coroutine_accessors1SV3irmSivxTwc
//           :    ),
// CHECK-SAME:    i32 0
// CHECK-SAME:  }>

// CHECK-arm64e-LABEL: _swift_coro_malloc.ptrauth = private constant {
// CHECK-arm64e-SAME:    ptr @_swift_coro_malloc,
// CHECK-arm64e-SAME:    i32 0,
// CHECK-arm64e-SAME:    i64 ptrtoint (
// CHECK-arm64e-SAME:      ptr getelementptr inbounds (
// CHECK-arm64e-SAME:        ptr @_swift_coro_malloc_allocator,
// CHECK-arm64e-SAME:        i32 0,
// CHECK-arm64e-SAME:        i32 1
// CHECK-arm64e-SAME:      )
// CHECK-arm64e-SAME:    )
// CHECK-arm64e-SAME:    i64 24469 }
// CHECK-arm64e-SAME:  section "llvm.ptrauth"
// CHECK-arm64e-SAME:  align 8
// CHECK-arm64e-LABEL: _swift_coro_free.ptrauth = private constant {
// CHECK-arm64e-SAME:    ptr @_swift_coro_free,
// CHECK-arm64e-SAME:    i32 0,
// CHECK-arm64e-SAME:    i64 ptrtoint (
// CHECK-arm64e-SAME:      ptr getelementptr inbounds (
// CHECK-arm64e-SAME:        ptr @_swift_coro_malloc_allocator,
// CHECK-arm64e-SAME:        i32 0,
// CHECK-arm64e-SAME:        i32 2
// CHECK-arm64e-SAME:      )
// CHECK-arm64e-SAME:    )
// CHECK-arm64e-SAME:    i64 40879 },
// CHECK-arm64e-SAME:  section "llvm.ptrauth",
// CHECK-arm64e-SAME:  align 8
// CHECK-arm64e-LABEL: _swift_coro_malloc.ptrauth.{{.*}} = private constant {
// CHECK-arm64e-SAME:    ptr @_swift_coro_malloc,
// CHECK-arm64e-SAME:    i32 0,
// CHECK-arm64e-SAME:    i64 ptrtoint (
// CHECK-arm64e-SAME:      ptr getelementptr inbounds (
// CHECK-arm64e-SAME:        ptr @_swift_coro_malloc_allocator,
// CHECK-arm64e-SAME:        i32 0,
// CHECK-arm64e-SAME:        i32 3
// CHECK-arm64e-SAME:      )
// CHECK-arm64e-SAME:    )
// CHECK-arm64e-SAME:    i64 53841 }
// CHECK-arm64e-SAME:  section "llvm.ptrauth"
// CHECK-arm64e-SAME:  align 8
// CHECK-arm64e-LABEL: _swift_coro_free.ptrauth.{{.*}} = private constant {
// CHECK-arm64e-SAME:    ptr @_swift_coro_free,
// CHECK-arm64e-SAME:    i32 0,
// CHECK-arm64e-SAME:    i64 ptrtoint (
// CHECK-arm64e-SAME:      ptr getelementptr inbounds (
// CHECK-arm64e-SAME:        ptr @_swift_coro_malloc_allocator,
// CHECK-arm64e-SAME:        i32 0,
// CHECK-arm64e-SAME:        i32 4
// CHECK-arm64e-SAME:      )
// CHECK-arm64e-SAME:    )
// CHECK-arm64e-SAME:    i64 23464 },
// CHECK-arm64e-SAME:  section "llvm.ptrauth",
// CHECK-arm64e-SAME:  align 8
// CHECK-LABEL: _swift_coro_malloc_allocator = linkonce_odr hidden constant %swift.coro_allocator {
// CHECK-SAME:       i32 258,
// CHECK-SAME:       _swift_coro_malloc
// CHECK-ar64e-SAME:     .ptrauth
// CHECK-SAME:       _swift_coro_free
// CHECK-ar64e-SAME:     .ptrauth
// CHECK-SAME:       _swift_coro_malloc
// CHECK-ar64e-SAME:     .ptrauth.{{.*}}
// CHECK-SAME:       _swift_coro_free
// CHECK-ar64e-SAME:     .ptrauth.{{.*}}
// CHECK-SAME:  }
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
// CHECK-arm64e-LABEL: _swift_coro_task_alloc.ptrauth.{{.*}} = private constant {
// CHECK-arm64e-SAME:    ptr @_swift_coro_task_alloc,
// CHECK-arm64e-SAME:    i32 0,
// CHECK-arm64e-SAME:    i64 ptrtoint (
// CHECK-arm64e-SAME:      ptr getelementptr inbounds (
// CHECK-arm64e-SAME:        ptr @_swift_coro_async_allocator,
// CHECK-arm64e-SAME:        i32 0,
// CHECK-arm64e-SAME:        i32 3
// CHECK-arm64e-SAME:      )
// CHECK-arm64e-SAME:    )
// CHECK-arm64e-SAME:    i64 53841 }
// CHECK-arm64e-SAME:  section "llvm.ptrauth"
// CHECK-arm64e-SAME:  align 8
// CHECK-arm64e-LABEL: @_swift_coro_task_dealloc.ptrauth.{{.*}} = private constant {
// CHECK-arm64e-SAME:    ptr @_swift_coro_task_dealloc,
// CHECK-arm64e-SAME:    i32 0,
// CHECK-arm64e-SAME:    i64 ptrtoint (
// CHECK-arm64e-SAME:      ptr getelementptr inbounds (
// CHECK-arm64e-SAME:        ptr @_swift_coro_async_allocator,
// CHECK-arm64e-SAME:        i32 0,
// CHECK-arm64e-SAME:        i32 4
// CHECK-arm64e-SAME:      )
// CHECK-arm64e-SAME:    )
// CHECK-arm64e-SAME:    i64 23464 },
// CHECK-arm64e-SAME:  section "llvm.ptrauth",
// CHECK-arm64e-SAME:  align 8
// CHECK-LABEL: _swift_coro_async_allocator = linkonce_odr hidden constant %swift.coro_allocator {
// CHECK-SAME:      i32 1,
// CHECK-SAME:      _swift_coro_task_alloc
// CHECK-ar64e-SAME:     .ptrauth
// CHECK-SAME:      _swift_coro_task_dealloc
// CHECK-ar64e-SAME:     .ptrauth
// CHECK-SAME:      _swift_coro_task_alloc
// CHECK-ar64e-SAME:     .ptrauth.{{.*}}
// CHECK-SAME:      _swift_coro_task_dealloc
// CHECK-ar64e-SAME:     .ptrauth.{{.*}}
// CHECK-SAME:  }

// CHECK-LABEL: @_swift_coro_alloc(
// CHECK-SAME:      ptr [[FRAME:%[^,]+]]
// CHECK-SAME:      ptr swiftcoro [[ALLOCATOR:%[^,]+]]
// CHECK-SAME:      [[INT]] [[SIZE:%[^)]+]]
// CHECK-SAME:  )
// CHECK-SAME:  {
// CHECK:       entry:
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
// CHECK:         [[ALLOCATION:%[^,]+]] = call swiftcc ptr [[ALLOCATE_FN]](ptr [[FRAME]], ptr swiftcoro [[ALLOCATOR]], [[INT]] [[SIZE]])
// CHECK:         ret ptr [[ALLOCATION]]
// CHECK:       }

// CHECK-LABEL: @_swift_coro_dealloc(
// CHECK-SAME:      ptr [[FRAME:%[^,]+]]
// CHECK-SAME:      ptr swiftcoro [[ALLOCATOR:%[^,]+]]
// CHECK-SAME:      ptr [[ADDRESS:%[^)]+]]
// CHECK-SAME:  )
// CHECK-SAME:  {
// CHECK:       entry:
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

@frozen
public struct S {
public var o: any AnyObject
public var _i: Int = 0

public var irm: Int {
// CHECK-LABEL: declare{{.*}} swiftcc void @"$s19coroutine_accessors1SVSiIetMIlYl_TC"(ptr noalias, ptr swiftcoro)

// CHECK-LABEL:     define{{.*}} { ptr, {{i64|i32}} } @"$s19coroutine_accessors1SV3irmSivy"(
// CHECK-SAME:          ptr noalias [[FRAME:%[^,]+]],
// CHECK-SAME:          ptr swiftcoro [[ALLOCATOR:%[^,]+]],
// CHECK-SAME:          ptr [[S_FIELD_O:%[^,]+]],
// CHECK-SAME:          [[INT]] [[S_FIELD__I:%[^)]+]]
// CHECK-SAME:      )
// CHECK-SAME:      {
// CHECK:               [[ID:%[^,]+]] = call token @llvm.coro.id.retcon.once.dynamic(
// CHECK-SAME:                   i32 -1,
// CHECK-SAME:                   i32 16,
// CHECK-SAME:                   ptr @"$s19coroutine_accessors1SV3irmSivyTwc",
// CHECK-SAME:                   ptr [[ALLOCATOR]],
// CHECK-SAME:                   ptr [[FRAME]],
// CHECK-SAME:                   $s19coroutine_accessors1SVSiIetMIgYy_TC
// CHECK-SAME:                   ptr @_swift_coro_alloc,
// CHECK-SAME:                   ptr @_swift_coro_dealloc
// CHECK-SAME:               )
// CHECK:               [[HANDLE:%[^,]+]] = call ptr @llvm.coro.begin(
// CHECK-SAME:                   token [[ID]],
// CHECK-SAME:                   ptr null
// CHECK-SAME:               )
// CHECK:               call ptr (...) @llvm.coro.suspend.retcon.p0([[INT]] [[S_FIELD__I]])
// CHECK:               br i1 false, label %[[UNWIND:[^,]+]], label %[[NORMAL:[^,]+]]
// CHECK:             [[NORMAL]]:
// CHECK:               br label %coro.end
// CHECK:             [[UNWIND]]:
// CHECK:               br label %coro.end
// CHECK:             coro.end:
// CHECK:               call i1 @llvm.coro.end(
// CHECK-SAME:              ptr [[HANDLE]],
// CHECK-SAME:              i1 false,
// CHECK-SAME:              token none
// CHECK-SAME:          )
// CHECK:               unreachable
// CHECK:           }
  yielding borrow {
    yield _i
  }
// CHECK-LABEL:     define{{.*}} { ptr, ptr } @"$s19coroutine_accessors1SV3irmSivx"(
// CHECK-SAME:          ptr noalias [[FRAME:%[^,]+]],
// CHECK-SAME:          ptr swiftcoro [[ALLOCATOR:%[^,]+]],
// CHECK-SAME:          ptr swiftself captures(none) dereferenceable({{8|16}}) [[SELF:%[^)]+]]
// CHECK-SAME:      )
// CHECK-SAME:      {
// CHECK:               [[ID:%[^,]+]] = call token @llvm.coro.id.retcon.once.dynamic(
// CHECK-SAME:                   i32 -1,
// CHECK-SAME:                   i32 16,
// CHECK-SAME:                   ptr @"$s19coroutine_accessors1SV3irmSivxTwc",
// CHECK-SAME:                   ptr [[ALLOCATOR]],
// CHECK-SAME:                   ptr [[FRAME]],
// CHECK-SAME:                   $s19coroutine_accessors1SVSiIetMIlYl_TC
// CHECK-SAME:                   ptr @_swift_coro_alloc,
// CHECK-SAME:                   ptr @_swift_coro_dealloc
// CHECK-SAME:               )
// CHECK:               [[HANDLE:%[^,]+]] = call ptr @llvm.coro.begin(
// CHECK-SAME:                   token [[ID]],
// CHECK-SAME:                   ptr null
// CHECK-SAME:               )
// CHECK:               [[S_FIELD__I:%[^,]+]] = getelementptr inbounds{{.*}} %T19coroutine_accessors1SV,
// CHECK-SAME:                     ptr [[SELF]],
// CHECK-SAME:                     i32 0,
// CHECK-SAME:                     i32 1
// CHECK:               call ptr (...) @llvm.coro.suspend.retcon.p0(
// CHECK-SAME:              ptr [[S_FIELD__I]]
// CHECK-SAME:          )
// CHECK:               br i1 false, label %[[UNWIND:[^,]+]], label %[[NORMAL:[^,]+]]
// CHECK:             [[NORMAL]]:
// CHECK:               br label %coro.end
// CHECK:             [[UNWIND]]:
// CHECK:               br label %coro.end
// CHECK:             coro.end:
// CHECK:               [[REGISTER_8:%[^,]+]] = call i1 @llvm.coro.end(ptr [[HANDLE]], i1 false, token none)
// CHECK:               unreachable
// CHECK:           }
  yielding mutate {
    yield &_i
  }
} // public var irm

// CHECK-LABEL: define{{.*}} void @increment_irm(
// CHECK-SAME:  )
// CHECK-SAME:  {
//      :         [[SIZE_32:%[^,]+]] = load i32
//           :        ptr getelementptr inbounds (
//           :            %swift.coro_func_pointer
// CHECK:                 $s19coroutine_accessors1SV3irmSivxTwc
//           :            i32 0
//           :            i32 1
//           :        )
// CHECK-64:      [[SIZE_64:%[^,]+]] = zext i32 {{%[^,]+}} to i64
// CHECK-64:      [[FRAME:%[^,]+]] = alloca i8, [[INT]] [[SIZE_64]]
// CHECK-32:      [[FRAME:%[^,]+]] = alloca i8, [[INT]] {{%[^,]+}}
// CHECK:         call void @llvm.lifetime.start.p0(i64 -1, ptr [[FRAME]])
// CHECK:         [[RAMP:%[^,]+]] = call ptr @llvm.coro.prepare.retcon(ptr @"$s19coroutine_accessors1SV3irmSivx")
// CHECK:         [[RETVAL:%[^,]+]] = call swiftcc { ptr, ptr } [[RAMP]](
// CHECK-SAME:         [[FRAME]],
// CHECK-SAME:         _swift_coro_malloc_allocator
// CHECK-SAME:    )
// CHECK:         [[CONTINUATION:%[^,]+]] = extractvalue { ptr, ptr } [[RETVAL]], 0
// CHECK:         [[YIELD:%[^,]+]] = extractvalue { ptr, ptr } [[RETVAL]], 1
// CHECK:         call swiftcc void @"$s19coroutine_accessors9incrementyySizF"(
// CHECK-SAME:        [[YIELD]]
// CHECK-SAME:    )
// CHECK:         call swiftcc void [[CONTINUATION]](
// CHECK-SAME:        [[FRAME]],
// CHECK-SAME:        _swift_coro_malloc_allocator
// CHECK-SAME:    )
// CHECK:         call void @llvm.lifetime.end.p0(i64 -1, ptr [[FRAME]])
// CHECK:       }
@_silgen_name("increment_irm")
public mutating func increment_irm() {
  increment(&irm)
}

// CHECK-LABEL: define{{.*}} void @increment_irm_async(
//                  ptr swiftasync %0
// CHECK-SAME:  )
// CHECK-SAME:  {
//      :         [[SIZE_32:%[^,]+]] = load i32
//           :        ptr getelementptr inbounds (
//           :            %swift.coro_func_pointer
// CHECK:                 $s19coroutine_accessors1SV3irmSivxTwc
//           :            i32 0
//           :            i32 1
//           :        )
// CHECK-64:      [[SIZE_64:%[^,]+]] = zext i32 {{%[^,]+}} to i64
// CHECK-64:      [[SIZE_1:%[^,]+]] = add [[INT]] [[SIZE_64]], 15
// CHECK-32:      [[SIZE_1:%[^,]+]] = add [[INT]] {{%[^,]+}}, 15
// CHECK:         [[SIZE:%[^,]+]] = and [[INT]] [[SIZE_1]], -16
// CHECK:         [[FRAME:%[^,]+]] = call swiftcc ptr @swift_task_alloc([[INT]] [[SIZE]])
// CHECK:         call void @llvm.lifetime.start.p0(i64 -1, ptr [[FRAME]])
// CHECK:         [[RAMP:%[^,]+]] = call ptr @llvm.coro.prepare.retcon(ptr @"$s19coroutine_accessors1SV3irmSivx")
// CHECK:         [[RETVAL:%[^,]+]] = call swiftcc { ptr, ptr } [[RAMP]](
// CHECK-SAME:         [[FRAME]],
// CHECK-SAME:         _swift_coro_async_allocator
// CHECK-SAME:    )
// CHECK:         [[CONTINUATION:%[^,]+]] = extractvalue { ptr, ptr } [[RETVAL]], 0
// CHECK:         [[YIELD:%[^,]+]] = extractvalue { ptr, ptr } [[RETVAL]], 1

//                increment_async([[YIELD]])

// CHECK:         call swiftcc void [[CONTINUATION]](
// CHECK-SAME:        [[FRAME]],
// CHECK-SAME:        _swift_coro_async_allocator
// CHECK-SAME:    )
// CHECK:         call void @llvm.lifetime.end.p0(i64 -1, ptr [[FRAME]])
// CHECK:         call swiftcc void @{{(_)?}}swift_task_dealloc_through(ptr [[FRAME]])
// CHECK:       }
@_silgen_name("increment_irm_async")
public mutating func increment_irm_async() async {
  await increment_async(&irm)
}

public var force_yield_once_convention : () {
  _read {
    let nothing: () = ()
    yield nothing
  }
// CHECK-LABEL: define{{.*}} { ptr, ptr } @increment_irm_yield_once(
//                  ptr noalias dereferenceable(32) %0
// CHECK-SAME:  )
// CHECK-SAME:  {
//      :         [[SIZE_32:%[^,]+]] = load i32
//           :        ptr getelementptr inbounds (
//           :            %swift.coro_func_pointer
//           :            $s19coroutine_accessors1SV3irmSivxTwc
//           :            i32 0
//           :            i32 1
//           :        )
// CHECK:         [[TYPE_ID:%[^,]+]] = load i64
// CHECK-64:      [[SIZE_64:%[^,]+]] = zext i32 {{%[^,]+}} to i64
// CHECK-64:      [[ALLOCATION:%[^,]+]] = call token{{.*}} @llvm.coro.alloca.alloc.frame.i64(i64 [[SIZE_64]], i32 16, i64 [[TYPE_ID]])
// CHECK-32:      [[ALLOCATION:%[^,]+]] = call token{{.*}} @llvm.coro.alloca.alloc.frame.i32(i32 {{%[^,]+}}, i32 16, i64 [[TYPE_ID]])
// CHECK:         [[FRAME:%[^,]+]] = call ptr @llvm.coro.alloca.get(token [[ALLOCATION]])
// CHECK:         call void @llvm.lifetime.start.p0(i64 -1, ptr [[FRAME]])
// CHECK:         [[RAMP:%[^,]+]] = call ptr @llvm.coro.prepare.retcon(ptr @"$s19coroutine_accessors1SV3irmSivx")
// CHECK:         [[RETVAL:%[^,]+]] = call swiftcc { ptr, ptr } [[RAMP]](
// CHECK-SAME:         [[FRAME]],
// CHECK-apple-SAME:         _swift_coro_typed_malloc_allocator
// CHECK-SAME:    )
// CHECK:         [[CONTINUATION:%[^,]+]] = extractvalue { ptr, ptr } [[RETVAL]], 0
// CHECK:         [[YIELD:%[^,]+]] = extractvalue { ptr, ptr } [[RETVAL]], 1
// CHECK:         call swiftcc void @"$s19coroutine_accessors9incrementyySizF"(
// CHECK-SAME:        [[YIELD]]
// CHECK-SAME:    )
// CHECK:         call swiftcc void [[CONTINUATION]](
// CHECK-SAME:        [[FRAME]],
// CHECK-apple-SAME:        _swift_coro_typed_malloc_allocator
// CHECK-SAME:    )
// CHECK:         call void @llvm.lifetime.end.p0(i64 -1, ptr [[FRAME]])
// CHECK:         call void @llvm.coro.alloca.free.frame(token [[ALLOCATION]])
// CHECK:       }
  @_silgen_name("increment_irm_yield_once")
  _modify {
    increment(&irm)

    var nothing: () = ()
    yield &nothing
  }
}

public var force_yield_once_2_convention : () {
  yielding borrow {
    let nothing: () = ()
    yield nothing
  }
// CHECK-LABEL: define{{.*}} { ptr, ptr } @increment_irm_yield_once_2(
//                  ptr noalias %0
// CHECK-SAME:      ptr swiftcoro [[ALLOCATOR:%[^,]+]]
//                  ptr swiftself captures(none) dereferenceable(16) %2
// CHECK-SAME:  )
// CHECK-SAME:  {
//      :         [[SIZE_32:%[^,]+]] = load i32
//           :        ptr getelementptr inbounds (
//           :            %swift.coro_func_pointer
// CHECK:                 $s19coroutine_accessors1SV3irmSivxTwc
//           :            i32 0
//           :            i32 1
//           :        )
// CHECK:         [[TYPE_ID:%[^,]+]] = load i64
// CHECK-64:      [[SIZE_64:%[^,]+]] = zext i32 {{%[^,]+}} to i64
// CHECK-64:      [[ALLOCATION:%[^,]+]] = call token{{.*}} @llvm.coro.alloca.alloc.frame.i64(i64 [[SIZE_64]], i32 16, i64 [[TYPE_ID]])
// CHECK-32:      [[ALLOCATION:%[^,]+]] = call token{{.*}} @llvm.coro.alloca.alloc.frame.i32(i32 {{%[^,]+}}, i32 16, i64 [[TYPE_ID]])
// CHECK:         [[FRAME:%[^,]+]] = call ptr @llvm.coro.alloca.get(token [[ALLOCATION]])
// CHECK:         call void @llvm.lifetime.start.p0(i64 -1, ptr [[FRAME]])
// CHECK:         [[RAMP:%[^,]+]] = call ptr @llvm.coro.prepare.retcon(ptr @"$s19coroutine_accessors1SV3irmSivx")
// CHECK:         [[RETVAL:%[^,]+]] = call swiftcc { ptr, ptr } [[RAMP]](
// CHECK-SAME:         [[FRAME]],
// CHECK-SAME:         [[ALLOCATOR]]
// CHECK-SAME:    )
// CHECK:         [[CONTINUATION:%[^,]+]] = extractvalue { ptr, ptr } [[RETVAL]], 0
// CHECK:         [[YIELD:%[^,]+]] = extractvalue { ptr, ptr } [[RETVAL]], 1
// CHECK:         call swiftcc void @"$s19coroutine_accessors9incrementyySizF"(
// CHECK-SAME:        [[YIELD]]
// CHECK-SAME:    )
// CHECK:         call swiftcc void [[CONTINUATION]](
// CHECK-SAME:        [[FRAME]],
// CHECK-SAME:        [[ALLOCATOR]]
// CHECK-SAME:    )
// CHECK:         call void @llvm.lifetime.end.p0(i64 -1, ptr [[FRAME]])
// CHECK:         call void @llvm.coro.alloca.free.frame(token [[ALLOCATION]])
// CHECK:       }
  @_silgen_name("increment_irm_yield_once_2")
  yielding mutate {
    increment(&irm)

    var nothing: () = ()
    yield &nothing
  }
}
}

public func increment_async(_ int: inout Int) async {
  int += 1
}

public func increment(_ int: inout Int) {
  int += 1
}
