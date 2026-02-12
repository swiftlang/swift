// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking %import-libdispatch -parse-as-library) | %FileCheck %s --dump-input=always

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: synchronization
// REQUIRES: libdispatch

// rdar://76038845
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

import Synchronization
import Dispatch

@available(SwiftStdlib 6.4, *)
func test_task_cancel_shield() async {
  print("==== ------------------------------------------------")
  print(#function)  // CHECK: test_task_cancel_shield

  let t = Task {
    print("Inside task, before cancel: isCancelled:\(Task.isCancelled)")  
    // CHECK: Inside task, before cancel: isCancelled:false

    print("cancel self")
    withUnsafeCurrentTask { $0?.cancel() }  // cancel myself!

    print("Inside task, after cancel: isCancelled:\(Task.isCancelled)")  
    // CHECK: Inside task, after cancel: isCancelled:true

    withTaskCancellationShield {
      print("Inside task, shielded: isCancelled:\(Task.isCancelled)") 
       // CHECK: Inside task, shielded: isCancelled:false
    }
    print("Inside task, after shield: isCancelled:\(Task.isCancelled)") 
     // CHECK: Inside task, after shield: isCancelled:true

    await withTaskCancellationShield {
      await withTaskCancellationHandler {
        print("shielded, withTaskCancellationHandler - operation")  
        // CHECK: shielded, withTaskCancellationHandler - operation

      } onCancel: {
        // MUST NOT execute because we're shielded
        print("shielded, withTaskCancellationHandler - onCancel") 
         // CHECK-NOT: shielded, withTaskCancellationHandler - onCancel
      }
    }

    async let child = {
      print("Inside child-task: isCancelled:\(Task.isCancelled)")  
      // CHECK: Inside child-task: isCancelled:true
      withTaskCancellationShield {
        print("Inside child-task, shielded: isCancelled:\(Task.isCancelled)")  
        // CHECK: Inside child-task, shielded: isCancelled:false
      }
    }()
    _ = await child

    let t2 = Task {
      let unsafeT2 = withUnsafeCurrentTask { $0! }  // escape self reference unsafely

      await withTaskCancellationShield {
        await withTaskCancellationHandler {
          print("cancel self")
          unsafeT2.cancel()  // cancel self, but we're shielded, so the handler MUST NOT run anyway
          // CHECK-NOT: Task{}.cancel, withTaskCancellationHandler - onCancel
          print("Task{}.cancel, shielded, withTaskCancellationHandler - operation")
          // CHECK: Task{}.cancel, shielded, withTaskCancellationHandler - operation
        } onCancel: {
          // MUST NOT execute because we're shielded
          print("Task{}.cancel, withTaskCancellationHandler - onCancel")
          // CHECK-NOT: Task{}.cancel, withTaskCancellationHandler - onCancel
        }
      }
    }
    await t2.value
  }

  await t.value
}

@available(SwiftStdlib 6.4, *)
func test_defer_cancel_shield() async {
  print("==== ------------------------------------------------")
  print(#function) // CHECK: test_defer_cancel_shield

  let task = Task {
    withUnsafeCurrentTask { $0?.cancel() }

    print("Inside task, before cancel: isCancelled:\(Task.isCancelled)")
    // CHECK: Inside task, before cancel: isCancelled:true

    defer {
      defer {
        print("Inside defer, after cancel: isCancelled:\(Task.isCancelled)")
        // CHECK: Inside defer, after cancel: isCancelled:true

        withTaskCancellationShield {
          print("Inside defer, shielded, after cancel: isCancelled:\(Task.isCancelled)")
          // CHECK: Inside defer, shielded, after cancel: isCancelled:false
        }

        print("Inside defer, after cancel: isCancelled:\(Task.isCancelled)")
        // CHECK: Inside defer, after cancel: isCancelled:true
      }
    }
  }

  await task.value
}

@available(SwiftStdlib 6.4, *)
func test_nested_shields() async {
  print("==== ------------------------------------------------")
  print(#function)  // CHECK: test_nested_shields

  let task = Task {
    withUnsafeCurrentTask { $0?.cancel() }
    print("Before shield, after cancel: isCancelled:\(Task.isCancelled)")
    // CHECK: Before shield, after cancel: isCancelled:true

    withTaskCancellationShield {
      print("Inside shield, after cancel: isCancelled:\(Task.isCancelled)")
      // CHECK: Inside shield, after cancel: isCancelled:false
      
      withTaskCancellationShield {
        print("Inside shield shield, after cancel: isCancelled:\(Task.isCancelled)")
        // CHECK: Inside shield shield, after cancel: isCancelled:false
      }

      // make sure we didn't remove the "outer" shield by accident
      print("Popped shield, still shielded, after cancel: isCancelled:\(Task.isCancelled)")
      // CHECK: Popped shield, still shielded, after cancel: isCancelled:false
    }

    print("Inside task, after cancel: isCancelled:\(Task.isCancelled)")
    // CHECK: Inside task, after cancel: isCancelled:true
  }

  await task.value
}

@available(SwiftStdlib 6.4, *)
func test_sleep_cancel_shield() async {
  print("==== ------------------------------------------------")
  print(#function)  // CHECK: test_sleep_cancel_shield

  let task = Task {
    await withTaskCancellationShield {
      try! await Task.sleep(for: .seconds(1))
    }
  }

  task.cancel()
  await task.value
}

@available(SwiftStdlib 6.4, *)
func test_async_stream_cancel_shield() async {
  print("==== ------------------------------------------------")
  print(#function)  // CHECK: test_async_stream_cancel_shield
  
  let (stream, continuation) = AsyncThrowingStream<Void, any Error>.makeStream()
  let task = Task {
    try await withTaskCancellationShield {
      for try await value in stream {
        print("Inside loop")
        // CHECK: Inside loop
      }
    }
  }

  task.cancel()
  continuation.yield(())
  continuation.finish()
  try! await task.value
}

@available(SwiftStdlib 6.4, *)
func test_child_task_cancel_shield() async {
  print("==== ------------------------------------------------")
  print(#function)  // CHECK: test_child_task_cancel_shield

  let cancellableContinuation = CancellableContinuation()
  await withTaskGroup { group in
    group.addTask {
      await withTaskCancellationShield {
        try! await cancellableContinuation.wait()
      }
    }

    group.cancelAll()
    cancellableContinuation.complete()
    print("cancellableContinuation.complete()")
  }
}

@available(SwiftStdlib 6.4, *)
func test_task_group_cancel_shield() async {
  print("==== ------------------------------------------------")
  print(#function)  // CHECK: test_task_group_cancel_shield

  await withTaskGroup(of: Void.self) { group in
    group.cancelAll()
    group.addTask {
      print("Outside cancellation shield, isCancelled:\(Task.isCancelled)")
      // CHECK: Outside cancellation shield, isCancelled:true

      await withTaskCancellationShield {
        print("Inside cancellation shield, isCancelled:\(Task.isCancelled)")
        // CHECK: Inside cancellation shield, isCancelled:false

        await withTaskGroup { group in
          print("Inside task group, isCancelled:\(Task.isCancelled)")
          // CHECK: Inside task group, isCancelled:false

          group.addTask {
            print("Inside child task, isCancelled:\(Task.isCancelled)")
            // CHECK: Inside child task, isCancelled:false
          }
        }
      }
    }
  }
}

@available(SwiftStdlib 6.4, *)
func test_add_task_cancel_shield() async {
  print("==== ------------------------------------------------")
  print(#function)  // CHECK: test_add_task_cancel_shield

  await withTaskGroup(of: Void.self) { group in
    group.cancelAll()
    // The cancellation shield is only protecting the addTask but not the child task
    withTaskCancellationShield {
      group.addTask {
        print("Inside child task, isCancelled:\(Task.isCancelled)")
        // CHECK: Inside child task, isCancelled:true
      }
    }
    group.addTask {
      print("Inside child task, isCancelled:\(Task.isCancelled)")
      // CHECK: Inside child task, isCancelled:true
    }
  }
}

@available(SwiftStdlib 6.4, *)
func test_hasActiveCancellationShield() async {
  print("==== ------------------------------------------------")
  print(#function)  // CHECK: test_hasActiveCancellationShield

  let task = Task {
    print("hasShield:\(Task.hasActiveCancellationShield)")
    // CHECK: hasShield:false

    withUnsafeCurrentTask { $0?.cancel() }
    print("isCancelled:\(Task.isCancelled)")
    // CHECK: isCancelled:true

    await withTaskCancellationShield {
      print("shielded isCancelled:\(Task.isCancelled), hasShield:\(Task.hasActiveCancellationShield)")
      // CHECK: shielded isCancelled:false, hasShield:true

      async let childAsync: Void = {
        print("async let isCancelled:\(Task.isCancelled), hasShield:\(Task.hasActiveCancellationShield)")
        // CHECK: async let isCancelled:false, hasShield:false
        withTaskCancellationShield {
          print("async let shielded isCancelled:\(Task.isCancelled), hasShield:\(Task.hasActiveCancellationShield)")
          // CHECK: async let shielded isCancelled:false, hasShield:true
        }
      }()
      await childAsync

      await withTaskGroup(of: Void.self) { group in
        group.addTask {
          print("group child isCancelled:\(Task.isCancelled), hasShield:\(Task.hasActiveCancellationShield)")
          // CHECK: group child isCancelled:false, hasShield:false
          withTaskCancellationShield {
            print("group child shielded isCancelled:\(Task.isCancelled), hasShield:\(Task.hasActiveCancellationShield)")
            // CHECK: group child shielded isCancelled:false, hasShield:true
          }
        }
      }

      await Task {
        print("unstructured isCancelled:\(Task.isCancelled), hasShield:\(Task.hasActiveCancellationShield)")
        // CHECK: unstructured isCancelled:false, hasShield:false
        await withTaskCancellationShield {
          print("unstructured shielded isCancelled:\(Task.isCancelled), hasShield:\(Task.hasActiveCancellationShield)")
          // CHECK: unstructured shielded isCancelled:false, hasShield:true
        }
      }.value
    }

    print("after shield isCancelled:\(Task.isCancelled), hasShield:\(Task.hasActiveCancellationShield)")
    // CHECK: after shield isCancelled:true, hasShield:false
  }

  await task.value
}

@available(SwiftStdlib 6.4, *)
func test_task_isCancelled_instance_vs_static() async {
  print("==== ------------------------------------------------")
  print(#function)  // CHECK: test_task_isCancelled_instance_vs_static

  let ready = DispatchSemaphore(value: 0)
  let proceed = DispatchSemaphore(value: 0)
  let insideShield = DispatchSemaphore(value: 0)
  let insideShieldCheckNow = DispatchSemaphore(value: 0)

  let task = Task {
    withUnsafeCurrentTask { $0?.cancel() }

    // CHECK: 1. Task.isCancelled (static): true
    print("1. Task.isCancelled (static): \(Task.isCancelled)")

    // Signal we're ready
    ready.signal()
    // Wait until cancelled
    proceed.wait()
    print("2. Task.isCancelled (static): \(Task.isCancelled)")

    await withTaskCancellationShield {
      print("3. Inside shield, Task.isCancelled (static): \(Task.isCancelled)")
      insideShieldCheckNow.signal()
      insideShield.wait()
    }

    print("5. After shield, Task.isCancelled (static): \(Task.isCancelled)")
  }

  // Wait for task to be ready (and has cancelled itself)
  ready.wait()

  // CHECK: 2. task.isCancelled (instance): true
  print("2. task.isCancelled (instance): \(task.isCancelled)")
  proceed.signal()

  insideShieldCheckNow.wait()
  // CHECK: 3. task.isCancelled (instance) (suspended while shielded): true
  print("3. task.isCancelled (instance) (suspended while shielded): \(task.isCancelled)")

  insideShield.signal()
  // CHECK: 5. After shield, Task.isCancelled (static): true

  await task.value

  // Instance property remains true after task completes
  print("task.isCancelled after completion: \(task.isCancelled)")
}

@available(SwiftStdlib 6.4, *)
struct CancellableContinuation: ~Copyable {
  let state = Mutex<(Bool, (CheckedContinuation<Void, any Error>?))>((false, nil))

  func wait() async throws {
    try await withTaskCancellationHandler {
      try await withCheckedThrowingContinuation { continuation in
        self.state.withLock { state -> CheckedContinuation<Void, any Error>? in
          if state.0 {
            return continuation
          } else {
            state.1 = continuation
            return nil
          }
        }?.resume(throwing: CancellationError())
      }
    } onCancel: {
      self.state.withLock { state in
        state.0 = true
        return state.1
      }?.resume(throwing: CancellationError())
    }
  }

  func complete() {
    var cc = self.state.withLock { $0.1 }
    while true {
      if let cc {
        cc.resume()
        return
      }
      cc = self.state.withLock { $0.1 }
    }
  }
}

@available(SwiftStdlib 6.4, *)
@main struct Main {
  static func main() async {
    await test_task_cancel_shield()
    await test_defer_cancel_shield()
    await test_nested_shields()
    await test_sleep_cancel_shield()
    await test_async_stream_cancel_shield()
    await test_child_task_cancel_shield()
    await test_task_group_cancel_shield()
    await test_add_task_cancel_shield()
    await test_hasActiveCancellationShield()
    await test_task_isCancelled_instance_vs_static()
    print("DONE")
  }
}
