// RUN: %target-swift-frontend -emit-irgen %s | %FileCheck %s

// REQUIRES: CPU=arm64e

public func callee() async -> Int {
    return 5
}

public func caller() async {
   let r = await callee()
   print(r)
}


// CHECK: define swifttailcc void @"$s31async_context_projection_arm64e6calleryyYaF"(ptr swiftasync %0)
// CHECK:  [[CURR_ASYNC_CTXT:%.*]] = alloca ptr
// CHECK:  [[SUSPEND_RES:%.*]] = call { ptr, i64 } (i32, ptr, ptr, ...) @llvm.coro.suspend.async.sl_p0i64s(i32 0, ptr {{.*}}, ptr @__swift_async_resume_project_context, ptr @"$s31async_context_projection_arm64e6calleryyYaF{{.*}}", ptr @"$s31async_context_projection_arm64e6calleeSiyYaF", ptr {{.*}})
// CHECK:  [[CALLEE_CTXT:%.*]] = extractvalue { ptr, i64 } [[SUSPEND_RES]], 0
// CHECK:  [[CALLER_CTXT:%.*]] = load ptr, ptr [[CALLEE_CTXT]]
// CHECK:  [[CALLER_CTXT_ADDR:%.*]] = ptrtoint ptr [[CALLEE_CTXT]] to i64
// CHECK:  [[DIS:%.*]] = call i64 @llvm.ptrauth.blend(i64 [[CALLER_CTXT_ADDR]], i64 48546)
// CHECK:  [[CALLER_CTXT_PTRAUTH:%.*]] = ptrtoint ptr [[CALLER_CTXT]] to i64
// CHECK:  [[CALLER_CTXT_AUTH:%.*]] = call i64 @llvm.ptrauth.auth(i64 [[CALLER_CTXT_PTRAUTH]], i32 2, i64 [[DIS]])
// CHECK:  [[CALLER_CTXT_PTR:%.*]] = inttoptr i64 [[CALLER_CTXT_AUTH]] to ptr
// CHECK:  store ptr [[CALLER_CTXT_PTR]], ptr [[CURR_ASYNC_CTXT]]
