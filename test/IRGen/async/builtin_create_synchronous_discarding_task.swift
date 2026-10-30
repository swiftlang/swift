// RUN: %target-swift-frontend -enable-builtin-module -primary-file %s -emit-sil -target %target-swift-6.2-abi-triple | %FileCheck %s --check-prefix=SIL
// RUN: %target-swift-frontend -enable-builtin-module -primary-file %s -emit-ir -target %target-swift-6.2-abi-triple | %FileCheck %s --check-prefix=IR
// RUN: not %target-swift-frontend -enable-builtin-module -primary-file %s -emit-sil -target %target-swift-6.2-abi-triple -D UNGUARDED 2>&1 | %FileCheck %s --check-prefix=UNGUARDED

// REQUIRES: concurrency
// REQUIRES: OS=macosx

// 'Builtin.createSynchronousDiscardingTask' lowers as the 'createAsyncTask' with a synchronous operation.

import Builtin
import _Concurrency

// The operation is passed to 'createAsyncTask' is a synchronous '() -> ()' function, no conversions etc.
// SIL-LABEL: sil [available {{.*}}] @$s42builtin_create_synchronous_discarding_task6launch5flags9operationySi_yycntF :
// SIL:       bb0(%0 : $Int, [[OP:%[0-9]+]] : $@callee_guaranteed () -> ()):
// SIL-NOT:     thunk
// SIL-NOT:     convert_function
// SIL:         builtin "createAsyncTask"(%0, {{%[0-9]+}}, {{%[0-9]+}}, {{%[0-9]+}}, {{%[0-9]+}}, {{%[0-9]+}}, [[OP]]) : $(Builtin.NativeObject, Builtin.RawPointer)
// SIL:       } // end sil function

// IRGen passes the flags, and the closure's function pointer and context
// unchanged: there is no async function pointer record. The result type is
// the one passed for any discarding task, the runtime does not use it.
// IR-LABEL: define {{.*}}swiftcc void @"$s42builtin_create_synchronous_discarding_task6launch5flags9operationySi_yycntF"(i64 %0, ptr %1, ptr %2)
// IR:         call swiftcc %swift.async_task_and_context @swift_task_create(i64 %0, ptr null, ptr {{.*}}@"$sytN"{{.*}}, ptr %1, ptr %2)
@available(SwiftStdlib 6.5, *)
public func launch(flags: Int, operation: sending @escaping () -> Void) {
  _ = Builtin.createSynchronousDiscardingTask(
    flags: flags,
    operation: operation)
}

#if UNGUARDED
// UNGUARDED: error: {{.*}}Builtin.createSynchronousDiscardingTask requires the synchronous operation task availability (Swift 6.5)
public func unguarded(operation: sending @escaping () -> Void) {
  _ = Builtin.createSynchronousDiscardingTask(flags: 0, operation: operation)
}
#endif
