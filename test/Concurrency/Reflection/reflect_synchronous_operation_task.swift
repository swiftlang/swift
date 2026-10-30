// RUN: %empty-directory(%t)
// RUN: %target-clang -x c %S/Inputs/reflect_synchronous_operation_task.c -o %t/shims.c.o -c
// RUN: %target-build-swift -c -target %target-swift-6.2-abi-triple -parse-as-library -enable-builtin-module -lswiftSwiftReflectionTest %s -o %t/main.swift.o -module-name reflect_synchronous_operation_task
// RUN: %target-build-swift %t/main.swift.o %t/shims.c.o -o %t/reflect_synchronous_operation_task
// RUN: %target-codesign %t/reflect_synchronous_operation_task

// RUN: %target-run %target-swift-reflection-test %t/reflect_synchronous_operation_task | %FileCheck %s --dump-input=always

// REQUIRES: reflection_test_support
// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: asan

// A task with a synchronous operation, no future fragment and no
// async context chain. The reflection library reports the operation
// as the task's run job and as its only async backtrace frame, and walks its
// small first task allocator slab like any other

import Builtin
@_spi(Concurrency) import _Concurrency
import Dispatch

import SwiftReflectionTest

@_silgen_name("getCurrentTaskShim")
func _getCurrentTaskShim() -> UInt

@_silgen_name("getFunctionPointerShim")
func _getFunctionPointerShim(_ function: () -> Void) -> UInt

enum Locals {
  @TaskLocal static var number: Int = 0
}

let operationDone = DispatchSemaphore(value: 0)

// The operation of the task
func syncOperation() {
  reflectionLog(str: "running syncOperation number=\(Locals.number)")
  reflect(asyncTask: _getCurrentTaskShim())
  operationDone.signal()
}

actor Blocker {
  func block(entered: DispatchSemaphore, release: DispatchSemaphore) {
    entered.signal()
    release.wait()
  }
}

func hex(_ value: UInt) -> String {
  "0x" + String(value, radix: 16)
}

func testSynchronousOperationTask() async {
  reflectionLog(str: "testSynchronousOperationTask")
  // CHECK: testSynchronousOperationTask

  // A reference to a global function is a function value with the function
  // itself as its function pointer and no context
  let operation = _getFunctionPointerShim(syncOperation)
  reflectionLog(str: "operation \(hex(operation))")
  // CHECK: operation [[OPERATION:0x[0-9a-f]+]]

  // Keep the actor busy, so that the new task stays enqueued on it
  let blocker = Blocker()
  let entered = DispatchSemaphore(value: 0)
  let release = DispatchSemaphore(value: 0)
  Task.detached {
    await blocker.block(entered: entered, release: release)
  }
  entered.wait()

  guard #available(SwiftStdlib 6.5, *) else {
    fatalError("This test shouldn't be running against old stdlibs.")
  }

  let flags = _taskCreateFlags(
    priority: nil, isChildTask: false, copyTaskLocals: true,
    inheritContext: false, enqueueJob: true,
    addPendingGroupTaskUnconditionally: false, isDiscardingTask: true,
    isSynchronousStart: false, hasSynchronousOperation: true)
  let executor = unsafeBitCast(blocker.unownedExecutor,
                               to: Builtin.Executor.self)
  let task: Builtin.NativeObject = Locals.$number.withValue(42) {
    let (task, _) = Builtin.createSynchronousDiscardingTask(
      flags: flags,
      initialSerialExecutor: executor,
      operation: syncOperation)
    return task
  }

  // The task is enqueued on the busy actor and did not run yet
  reflect(asyncTask: unsafeBitCast(task, to: UInt.self))
  // CHECK: Reflecting an async task.
  // CHECK: Async task {{0x[0-9a-f]+}}
  // CHECK: synchronousOperation
  // CHECK-NEXT: running false
  // CHECK-NEXT: runJob [[OPERATION]]
  // CHECK-NEXT: asyncBacktrace [[OPERATION]]
  // CHECK-NEXT: children = {}

  // Its first slab is the small slab that is tail-allocated with the task,
  // which holds the copied task local and the dependency record of the
  // enqueued task, not a regular slab of 1KB or more
  // CHECK-NEXT:   Slab pointer {{0x[0-9a-f]+}}
  // CHECK-NEXT:     Slab size {{[0-9]{2,3}$}}
  // CHECK-NEXT:     Chunk at {{0x[0-9a-f]+}} length {{[0-9]+}} kind {{[0-9]+}}
  // CHECK-NEXT:     Chunk at {{0x[0-9a-f]+}} length {{[1-9][0-9]*}} kind {{[0-9]+}}
  // CHECK-NOT:    Slab pointer
  // CHECK: running syncOperation number=42

  // Let the task run, it reflects itself from its operation
  release.signal()
  operationDone.wait()
  // CHECK: Reflecting an async task.
  // CHECK: Async task {{0x[0-9a-f]+}}
  // CHECK: synchronousOperation
  // CHECK-NEXT: running true
  // CHECK-NEXT: runJob [[OPERATION]]
  // CHECK-NEXT: children = {}

  withExtendedLifetime(task) {}
}

@main struct Main {
  static func main() async {
    await testSynchronousOperationTask()

    doneReflecting()
  }
}

// CHECK: Done.
