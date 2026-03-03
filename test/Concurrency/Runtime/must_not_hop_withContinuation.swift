// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple %import-libdispatch) | %FileCheck %s --dump-input=always
// RUN: %target-run-simple-swift( -O -target %target-swift-5.1-abi-triple %import-libdispatch) | %FileCheck %s --dump-input=always
// REQUIRES: concurrency
// REQUIRES: executable_test

// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: freestanding
// REQUIRES: libdispatch
// REQUIRES: synchronization

import Synchronization


if #available(SwiftStdlib 6.0, *) {
  print("=== foo() async")
  print("---------------------------------------")
  await foo()
  print("=== typedThrows() async")
  print("---------------------------------------")
  await typedThrows()
}

// CHECK: === foo() async
// CHECK-NEXT: ---------------------------------------
// We hop to the task executor:
// CHECK-NEXT: [executor][task-executor] Enqueue (1)

// CHECK-NEXT: foo - withTaskExecutorPreference

// CHECK: foo - withTaskExecutorPreference - withCheckedContinuation
// CHECK-NEXT: foo - withTaskExecutorPreference - withCheckedContinuation done

// CHECK: foo - withTaskExecutorPreference - withUnsafeContinuation
// CHECK-NEXT: foo - withTaskExecutorPreference - withUnsafeContinuation done

// CHECK: foo - withTaskExecutorPreference - withCheckedThrowingContinuation
// CHECK-NEXT: foo - withTaskExecutorPreference - withCheckedThrowingContinuation done

// CHECK: foo - withTaskExecutorPreference - withUnsafeThrowingContinuation
// CHECK-NEXT: foo - withTaskExecutorPreference - withUnsafeThrowingContinuation done

// By checking that this is the second enqueue here,
// we check that there was no stray enqueues between with... invocations:
// CHECK-NEXT: [executor][task-executor] Enqueue (2)

// CHECK-NEXT: foo - withTaskExecutorPreference done

// CHECK-NEXT: == Make: actor Foo
// CHECK-NEXT: ---------------------------------------
// CHECK-NEXT: [executor][actor-executor] Enqueue (1)
// CHECK-NEXT: actor.foo

// CHECK: actor.foo - withCheckedContinuation
// CHECK-NEXT: actor.foo - withCheckedContinuation done

// CHECK: actor.foo - withUnsafeContinuation
// CHECK-NEXT: actor.foo - withUnsafeContinuation done

// CHECK: actor.foo - withCheckedThrowingContinuation
// CHECK-NEXT: actor.foo - withCheckedThrowingContinuation done

// CHECK: actor.foo - withUnsafeThrowingContinuation
// CHECK-NEXT: actor.foo - withUnsafeThrowingContinuation done
// CHECK-NEXT: actor.foo done

// No more enqueues are expected afterwards
// CHECK-NOT: [executor]

// CHECK: === typedThrows() async
// CHECK-NEXT: ---------------------------------------
// CHECK-NEXT: withCheckedThrowingContinuation typed throws - resumed
// CHECK-NEXT: withCheckedThrowingContinuation typed throws - caught MyError
// CHECK-NEXT: withCheckedThrowingContinuation typed throws - caught Error, legacy style

// CHECK-NEXT: withUnsafeThrowingContinuation typed throws - resumed
// CHECK-NEXT: withUnsafeThrowingContinuation typed throws - caught MyError
// CHECK-NEXT: withUnsafeThrowingContinuation typed throws - caught Error, legacy style

@available(SwiftStdlib 6.0, *)
@concurrent
func foo() async {
  await withTaskExecutorPreference(EnqueueCountExecutor(name: "task-executor")) {
    print("foo - withTaskExecutorPreference")
    await withCheckedContinuation { cont in
      print("foo - withTaskExecutorPreference - withCheckedContinuation")
      cont.resume()
    }
    print("foo - withTaskExecutorPreference - withCheckedContinuation done")

    await withUnsafeContinuation { cont in
      print("foo - withTaskExecutorPreference - withUnsafeContinuation")
      cont.resume()
    }
    print("foo - withTaskExecutorPreference - withUnsafeContinuation done")

    try! await withCheckedThrowingContinuation { cont in
      print("foo - withTaskExecutorPreference - withCheckedThrowingContinuation")
      cont.resume()
    }
    print("foo - withTaskExecutorPreference - withCheckedThrowingContinuation done")

    try! await withUnsafeThrowingContinuation { cont in
      print("foo - withTaskExecutorPreference - withUnsafeThrowingContinuation")
      cont.resume()
    }
    print("foo - withTaskExecutorPreference - withUnsafeThrowingContinuation done")
  }
  print("foo - withTaskExecutorPreference done")

  print("== Make: actor Foo")
  print("---------------------------------------")
  await Foo().foo()
}

@available(SwiftStdlib 6.0, *)
actor Foo {
  let exec = EnqueueCountExecutor(name: "actor-executor")

  nonisolated var unownedExecutor: UnownedSerialExecutor {
    self.exec.asUnownedSerialExecutor()
  }

  func foo() async {
    print("actor.foo")

    await withCheckedContinuation { cont in
      print("actor.foo - withCheckedContinuation")
      cont.resume()
    }
    print("actor.foo - withCheckedContinuation done")

    await withUnsafeContinuation { cont in
      print("actor.foo - withUnsafeContinuation")
      cont.resume()
    }
    print("actor.foo - withUnsafeContinuation done")

    try! await withCheckedThrowingContinuation { cont in
      print("actor.foo - withCheckedThrowingContinuation")
      cont.resume()
    }
    print("actor.foo - withCheckedThrowingContinuation done")

    try! await withUnsafeThrowingContinuation { cont in
      print("actor.foo - withUnsafeThrowingContinuation")
      cont.resume()
    }
    print("actor.foo - withUnsafeThrowingContinuation done")

    print("actor.foo done")
  }
}

@available(SwiftStdlib 6.0, *)
struct MyError: Error {}

@available(SwiftStdlib 6.0, *)
@concurrent
func typedThrows() async {
  let value0: Int = try! await withCheckedThrowingContinuation { (cont: CheckedContinuation<Int, MyError>) in
    cont.resume(returning: 42)
  }
  assert(value0 == 42)
  let value1: Int = try! await withCheckedThrowingContinuation { cont in
    let _: CheckedContinuation<Int, any Error> = cont // check the type
    cont.resume(returning: 42)
  }
  assert(value1 == 42)
  print("withCheckedThrowingContinuation typed throws - resumed")

  do {
    let _: Int = try await withCheckedThrowingContinuation { (cont: CheckedContinuation<Int, MyError>) in
      cont.resume(throwing: MyError())
    }
    fatalError("should have thrown")
  } catch {
    // error is statically 'MyError' thanks to typed throws
    let _: MyError = error
    print("withCheckedThrowingContinuation typed throws - caught MyError")
  }

  do { // previous patterns still work, though remain untyped unless we spell out the failure
    let _: Int = try await withCheckedThrowingContinuation { cont in
      let _: CheckedContinuation<Int, any Error> = cont // check the type
      cont.resume(throwing: MyError())
    }
    fatalError("should have thrown")
  } catch {
    let _: Error = error
    print("withCheckedThrowingContinuation typed throws - caught Error, legacy style")
  }

  let value2: Int = try! await withUnsafeThrowingContinuation { (cont: UnsafeContinuation<Int, MyError>) in
    cont.resume(returning: 42)
  }
  assert(value2 == 42)
  let value3: Int = try! await withUnsafeThrowingContinuation { cont in
    let _: UnsafeContinuation<Int, any Error> = cont // check the type
    cont.resume(returning: 42)
  }
  assert(value3 == 42)
  print("withUnsafeThrowingContinuation typed throws - resumed")

  do {
    let _: Int = try await withUnsafeThrowingContinuation { (cont: UnsafeContinuation<Int, MyError>) in
      cont.resume(throwing: MyError())
    }
    fatalError("should have thrown")
  } catch {
    let _: MyError = error
    print("withUnsafeThrowingContinuation typed throws - caught MyError")
  }
  do { // previous patterns still work, though remain untyped unless we spell out the failure
    let _: Int = try await withUnsafeThrowingContinuation { cont in
      let _: UnsafeContinuation<Int, any Error> = cont // check the type
      cont.resume(throwing: MyError())
    }
    fatalError("should have thrown")
  } catch {
    let _: Error = error
    print("withUnsafeThrowingContinuation typed throws - caught Error, legacy style")
  }
}

@available(SwiftStdlib 6.0, *)
final class EnqueueCountExecutor: TaskExecutor, SerialExecutor {
  let enqueueCount: Atomic<Int>

  let name: String

  init(name: String) {
    self.enqueueCount = .init(0)
    self.name = name
  }

  public func enqueue(_ job: consuming ExecutorJob) {
    let newEnqueueValue = self.enqueueCount.add(1, ordering: .relaxed).newValue
    print("[executor][\(self.name)] Enqueue (\(newEnqueueValue))")
    job.runSynchronously(on: self.asUnownedSerialExecutor())
  }
}

print("done") // CHECK: done
