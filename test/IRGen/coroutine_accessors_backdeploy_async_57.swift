// RUN: %target-swift-emit-irgen                                            \
// RUN:     %s                                                              \
// RUN:     -enable-callee-allocated-coro-abi                               \
// RUN:     -module-name backdep                                            \
// RUN:     -target %target-swift-5.7-abi-triple                            \
// RUN:     -Onone                                                          \
// RUN:     -enable-experimental-feature CoroutineAccessors                 \
// RUN: | %IRGenFileCheck %s

// REQUIRES: swift_stable_abi
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// REQUIRES: swift_feature_CoroutineAccessors

// CHECK-64-LABEL: %swift.back_deploy.task.post_57 = type {
//                     object header
// CHECK-64-SAME:      %swift.refcounted
//                     Job.SchedulerPrivate
// CHECK-64-SAME:      ptr, ptr
//                     Job.Flags
// CHECK-64-SAME:      i32
//                     Job.ID
// CHECK-64-SAME:      i32
//                     Job.Voucher
// CHECK-64-SAME:      ptr
//                     Job.Reserved
// CHECK-64-SAME:      ptr
//                     Job.RunJob/Job.ResumeTask
// CHECK-64-SAME:      ptr
//                     Task.ResumeContext
// CHECK-64-SAME:      ptr
//                     #if SWIFT_POINTER_IS_8_BYTES
//                     Task.Reserved64
//                     #endif
// CHECK-64-SAME:      ptr
//                     Task.ExclusivityAccessSet
// CHECK-64-SAME:      [2 x [[INT]]]
//                     Task.StatusStorage
// CHECK-64-SAME:      [2 x ptr]
// CHECK-64-SAME:      ptr
// CHECK-64-SAME:  }

// CHECK-32-LABEL: %swift.back_deploy.task.post_57.escalating = type {
//                     object header
// CHECK-32-SAME:      %swift.refcounted
//                     Job.SchedulerPrivate
// CHECK-32-SAME:      ptr, ptr
//                     Job.Flags
// CHECK-32-SAME:      i32
//                     Job.ID
// CHECK-32-SAME:      i32
//                     Job.Voucher
// CHECK-32-SAME:      ptr
//                     Job.Reserved
// CHECK-32-SAME:      ptr
//                     Job.RunJob/Job.ResumeTask
// CHECK-32-SAME:      ptr
//                     Task.ResumeContext
// CHECK-32-SAME:      ptr
//                     Task.ExclusivityAccessSet
// CHECK-32-SAME:      [2 x i32],
//                     Task.StatusStorage
// CHECK-32-SAME:      [4 x ptr],
// CHECK-32-SAME:      ptr
// CHECK-32-SAME:  }
// CHECK-32-LABEL: %swift.back_deploy.task.post_57.nonescalating = type {
//                     object header
// CHECK-32-SAME:      %swift.refcounted
//                     Job.SchedulerPrivate
// CHECK-32-SAME:      ptr, ptr
//                     Job.Flags
// CHECK-32-SAME:      i32
//                     Job.ID
// CHECK-32-SAME:      i32
//                     Job.Voucher
// CHECK-32-SAME:      ptr
//                     Job.Reserved
// CHECK-32-SAME:      ptr
//                     Job.RunJob/Job.ResumeTask
// CHECK-32-SAME:      ptr
//                     Task.ResumeContext
// CHECK-32-SAME:      ptr
//                     Task.ExclusivityAccessSet
// CHECK-32-SAME:      [2 x i32],
//                     Task.StatusStorage
// CHECK-32-SAME:      [2 x ptr],
// CHECK-32-SAME:      ptr
// CHECK-32-SAME:  }

public struct S {
public var _i: Int = 0

public var i: Int {
  read {
    yield _i
  }
  modify {
    yield &_i
  }
}

// CHECK-LABEL: define{{.*}} void @increment_i_async(
//                  ptr swiftasync %0
// CHECK-SAME:      ptr{{( nocapture)?}} swiftself{{( captures\(none\))?}} dereferenceable({{8|4}}) %1
// CHECK-SAME:  )
// CHECK-SAME:  {
//      :         [[SIZE_32:%[^,]+]] = load i32
//           :        ptr getelementptr inbounds (
//           :            %swift.coro_func_pointer
// CHECK:                 $s7backdep1SV1iSivxTwc
//           :            i32 0
//           :            i32 1
//           :        )
// CHECK-64:      [[SIZE_64:%[^,]+]] = zext i32 {{%[^,]+}} to i64
// CHECK-64:      [[SIZE_1:%[^,]+]] = add [[INT]] [[SIZE_64]], 15
// CHECK-32:      [[SIZE_1:%[^,]+]] = add [[INT]] {{%[^,]+}}, 15
// CHECK:         [[SIZE:%[^,]+]] = and [[INT]] [[SIZE_1]], -16
// CHECK:         [[FRAME:%[^,]+]] = call swiftcc ptr @swift_task_alloc([[INT]] [[SIZE]])
// CHECK:         call void @llvm.lifetime.start.p0(i64 -1, ptr [[FRAME]])
// CHECK:         [[RAMP:%[^,]+]] = call ptr @llvm.coro.prepare.retcon(ptr @"$s7backdep1SV1iSivx")
// CHECK:         [[RETVAL:%[^,]+]] = call swiftcc { ptr, ptr } [[RAMP]](
// CHECK-SAME:         [[FRAME]]
// CHECK-SAME:         _swift_coro_async_allocator
// CHECK-SAME:         %1
// CHECK-SAME:    )
// CHECK:         [[CONTINUATION:%[^,]+]] = extractvalue { ptr, ptr } [[RETVAL]], 0
// CHECK:         [[YIELD:%[^,]+]] = extractvalue { ptr, ptr } [[RETVAL]], 1

//                increment_async([[YIELD]])

// CHECK:         call swiftcc void [[CONTINUATION]](
// CHECK-SAME:        [[FRAME]],
// CHECK-SAME:        _swift_coro_async_allocator
// CHECK-SAME:    )
// CHECK:         call void @llvm.lifetime.end.p0(i64 -1, ptr [[FRAME]])
// CHECK:         call swiftcc void @_swift_task_dealloc_through(ptr [[FRAME]])
// CHECK:       }
@_silgen_name("increment_i_async")
public mutating func increment_i_async() async {
  await increment_async(&i)
}
}

public func increment_async(_ int: inout Int) async {
  int += 1
}

// CHECK-LABEL: define{{.*}} void @_swift_task_dealloc_through(
// CHECK-SAME:      ptr [[FINAL_ALLOCATED_MEMORY:%[^)]+]]
// CHECK-SAME:  )
// CHECK-SAME:  {
// CHECK:       entry:
// CHECK:         [[HAS_RUNTIME_SYMBOL:%[^,]+]] = icmp ne ptr @swift_task_dealloc_through, null
// CHECK:         br i1 [[HAS_RUNTIME_SYMBOL]]
// CHECK-SAME:        label %runtime_symbol
// CHECK-SAME:        label %no_runtime_symbol
// CHECK:       runtime_symbol:
// CHECK:         call swiftcc void @swift_task_dealloc_through(ptr [[FINAL_ALLOCATED_MEMORY]])
// CHECK:         ret void
// CHECK:       no_runtime_symbol:
// CHECK:         [[TASK:%[^,]+]] = call swiftcc ptr @swift_task_getCurrent()

// CHECK-64:      [[ALLOCATION_ADDR_ADDR:%[^,]+]] = getelementptr inbounds
// CHECK-64-SAME:     %swift.back_deploy.task.post_57
// CHECK-64-SAME:     ptr [[TASK]],
// CHECK-64-SAME:     i32 0
// CHECK-64-SAME:     i32 12

// CHECK-32:      [[ESCALATION_IS_DEFINED:%[^,]+]] = icmp ne ptr @_swift_concurrency_debug_supportsPriorityEscalation, null
// CHECK-32:      br i1 [[ESCALATION_IS_DEFINED]]
// CHECK-32-SAME:     label %escalation_has_symbol
// CHECK-32-SAME:     label %escalation_no
// CHECK-32:    escalation_has_symbol:
// CHECK-32:      [[ESCALATION:%[^,]+]] = load i8, ptr @_swift_concurrency_debug_supportsPriorityEscalation
// CHECK-32:      [[ESCALATION_IS_TRUE:%[^,]+]] = icmp ne i8 [[ESCALATION]], 0
// CHECK-32:      br i1 [[ESCALATION_IS_TRUE]]
// CHECK-32-SAME:     label %escalation_yes
// CHECK-32-SAME:     label %escalation_no
// CHECK-32:    escalation_yes:
// CHECK-32:      [[YES_ADDR:%[^,]+]] = getelementptr inbounds
// CHECK-32-SAME:     %swift.back_deploy.task.post_57.escalating
// CHECK-32-SAME:     ptr [[TASK]]
// CHECK-32-SAME:     i32 0
// CHECK-32-SAME:     i32 11
// CHECK-32:      br label %escalation_merge
// CHECK-32:    escalation_no:
// CHECK-32:      [[NO_ADDR:%[^,]+]] = getelementptr inbounds
// CHECK-32-SAME:     %swift.back_deploy.task.post_57.nonescalating
// CHECK-32-SAME:     ptr [[TASK]]
// CHECK-32-SAME:     i32 0
// CHECK-32-SAME:     i32 11
// CHECK-32:      br label %escalation_merge
// CHECK-32:    escalation_merge:
// CHECK-32:      [[ALLOCATION_ADDR_ADDR:%[^,]+]] = phi ptr
// CHECK-32-SAME:     [ [[NO_ADDR]], %escalation_no ]
// CHECK-32-SAME:     [ [[YES_ADDR]], %escalation_yes ]

// CHECK:         br label %loop
// CHECK:       loop:
// CHECK:         [[ALLOCATION_ADDR:%[^,]+]] = load ptr
// CHECK-SAME:        ptr [[ALLOCATION_ADDR_ADDR]]
// CHECK:         [[ALLOCATED_MEMORY:%[^,]+]] = getelementptr inbounds i8
// CHECK-SAME:        ptr [[ALLOCATION_ADDR]]
// CHECK-SAME:        [[INT]] 16
// CHECK:         call swiftcc void @swift_task_dealloc(
// CHECK-SAME:        ptr [[ALLOCATED_MEMORY]]
// CHECK-SAME:    )
// CHECK:         [[WAS_FINAL:%[^,]+]] = icmp ne ptr
// CHECK-SAME:        [[ALLOCATED_MEMORY]]
// CHECK-SAME:        [[FINAL_ALLOCATED_MEMORY]]
// CHECK:         br i1 [[WAS_FINAL]]
// CHECK-SAME:        label %loop
// CHECK-SAME:        label %exit
// CHECK:       exit:
// CHECK:         ret void
// CHECK:       }
