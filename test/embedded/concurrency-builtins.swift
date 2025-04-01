// RUN: %target-swift-frontend -emit-irgen %s -enable-experimental-feature Embedded -enable-builtin-module | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded

import Builtin

public func test() async {
    _ = Builtin.createAsyncTask(0) { () async throws -> Int in
      return 42
    }
}

// CHECK: define {{.*}}@"$e4main4testyyYaF"(ptr swiftasync %0)
// CHECK: entry:
// CHECK:   %result_type_info_record = alloca %swift.result_type_info_task_option
// CHECK:   call {{.*}}@llvm.coro.id.async
// CHECK:   call {{.*}}@llvm.coro.begin
// CHECK:   call {{.*}}@llvm.coro.async.resume
// CHECK:   call {{.*}}@llvm.coro.suspend.async.sl_p0s
// CHECK:   call {{.*}}@__swift_async_resume_get_context
// CHECK:   call {{.*}}%swift.async_task_and_context @swift_task_create
// CHECK:   call {{.*}}@swift_release
// CHECK:   call {{.*}}@llvm.coro.end.async
// CHECK:   unreachable
// CHECK: }
