// RUN: %empty-directory(%t)
// RUN: %target-clang -x c %S/Inputs/reflect_task.c -o %t/reflect_task.c.o -c
// RUN: %target-build-swift -c -target %target-swift-5.1-abi-triple -parse-stdlib -parse-as-library -lswiftSwiftReflectionTest %s -o %t/reflect_task.swift.o -module-name reflect_task
// RUN: %target-build-swift %t/reflect_task.swift.o %t/reflect_task.c.o -o %t/reflect_task
// RUN: %target-codesign %t/reflect_task

// RUN: %target-run %target-swift-reflection-test %t/reflect_task | %FileCheck %s --dump-input=always

// REQUIRES: reflection_test_support
// REQUIRES: executable_test
// REQUIRES: concurrency
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: asan

import Swift
import _Concurrency

import SwiftReflectionTest

// We do not use swift_task_getCurrent directly since we also can get a
// declaration (if we get unlucky) from _Concurrency with a differing type. So
// we instead just compile a shim in a .c file that actually calls
// swift_task_getCurrent and avoid the collision.
@_silgen_name("getCurrentTaskShim")
func _getCurrentTaskShim() -> UInt

func add(_ a: UInt, _ b: UInt) async -> UInt {
  if b == 0 {
    reflect(asyncTask: _getCurrentTaskShim())
    return a
  } else {
    return await add(a, b - 1) + 1
  }
}

func sleepForever() async -> Int {
  if #available(SwiftStdlib 5.7, *) {
    try? await Task.sleep(for: .seconds(1_000_000_000))
    return 42
  } else {
    fatalError("This test shouldn't be running against old stdlibs.")
  }
}

func testNestedCallsTask() async {
  reflectionLog(str: "testNestedCallsTask")
  // CHECK: testNestedCallsTask

  let n = await add(100, 100)
  reflect(any: n)
  // CHECK: Reflecting an async task.
  // CHECK: Async task {{0x[0-9a-fA-F]*}}

  // The actual number of chunks we'll get depends on internal implementation
  // details that we don't want this test to depend on. We'll just make sure
  // we get at least two, and ignore the details.
  // CHECK:   Slab pointer {{0x[0-9a-fA-F]*}}
  // CHECK:     Slab size {{[0-9]{2,}()}}
  // CHECK:     Chunk at {{0x[0-9a-fA-F]*}} length {{[1-9][0-9]*}} kind {{[0-9]*}}
  // CHECK:   Slab pointer {{0x[0-9a-fA-F]*}}
  // CHECK:     Slab size {{[0-9]{2,}()}}
  // CHECK:     Chunk at {{0x[0-9a-fA-F]*}} length {{[1-9[[0-9]*}} kind {{[0-9]*}}
}

// ###HACK: Avoid a race condition by waiting a bit.
//
// If we reflect the current task immediately, there is a chance that the
// runtime is busy manipulating the task structure (e.g. allocating using the
// task allocator on a child task, or adding a child task) in parallel with
// the reflection.  This can cause this test to crash when the
// swift-reflection-test binary asks to read from an uninitialized address.
//
// While we could stop the crashing by changing the pipe memory reader to
// return error codes when it is unable to read memory, that would still
// result in this test failing randomly.  It's not obvious what the "proper"
// fix for this should be; the Concurrency runtime isn't going to export
// anything that would help here.
func dodgeRaceCondition() async {
  if #available(SwiftStdlib 5.7, *) {
    try! await Task.sleep(for: .milliseconds(250))
  } else {
    fatalError("This test shouldn't be running against old stdlibs.")
  }
}

func testOneAsyncLet() async {
  reflectionLog(str: "testOneAsyncLet")
  // CHECK: testOneAsyncLet

  async let alet = sleepForever()

  await dodgeRaceCondition()

  reflect(asyncTask: _getCurrentTaskShim())
  // CHECK: Async task {{0x[0-9a-fA-F]*}}
  // CHECK: children = {
  // CHECK:   Async task {{0x[0-9a-fA-F]*}}
  // CHECK: }
}

func testMultipleAsyncLet() async {
  reflectionLog(str: "testMultipleAsyncLet")
  // CHECK: testMultipleAsyncLet

  async let alet1 = sleepForever()
  async let alet2 = sleepForever()

  await dodgeRaceCondition()

  reflect(asyncTask: _getCurrentTaskShim())
  // CHECK: Async task {{0x[0-9a-fA-F]*}}
  // CHECK: children = {
  // CHECK:   Async task {{0x[0-9a-fA-F]*}}
  // CHECK:   Async task {{0x[0-9a-fA-F]*}}
  // CHECK: }
}

@main struct Main {
  static func main() async {
    await testNestedCallsTask()
    await testOneAsyncLet()
    await testMultipleAsyncLet()

    doneReflecting()
  }
}

// CHECK: Done.
