// RUN: %empty-directory(%t)

// RUN: %target-build-swift %s -import-objc-header %S/Inputs/has-dispatch-private.h -Xfrontend -disable-availability-checking -o %t/priority_escalation_thread_qos
// RUN: %target-codesign %t/priority_escalation_thread_qos
// RUN: %target-run %t/priority_escalation_thread_qos

// REQUIRES: OS=macosx || OS=ios || OS=tvos || OS=watchos || OS=xros
// REQUIRES: executable_test
// REQUIRES: concurrency

// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: back_deploy_concurrency

// rdar://101077408 - Temporarily disable on simulators
// UNSUPPORTED: DARWIN_SIMULATOR=watchos
// UNSUPPORTED: DARWIN_SIMULATOR=ios
// UNSUPPORTED: DARWIN_SIMULATOR=tvos
// UNSUPPORTED: DARWIN_SIMULATOR=xros

import Darwin
import Dispatch
import StdlibUnittest

let tests = TestSuite("Task and thread priority escalation")

// Swift complains if we use `wait()` in an async function. Hide it here.
func semaphoreWait(_ sem: DispatchSemaphore) {
  sem.wait()
}

func getEffectiveThreadPriority() -> Int32 {
  var extInfo = thread_extended_info_data_t()
  var count = mach_msg_type_number_t(
    MemoryLayout<thread_extended_info_data_t>.size / MemoryLayout<integer_t>.size)
  let thread = mach_thread_self()
  defer { mach_port_deallocate(mach_task_self_, thread) }

  let kr = withUnsafeMutablePointer(to: &extInfo) { ptr in
    ptr.withMemoryRebound(to: integer_t.self, capacity: Int(count)) { intPtr in
      thread_info(thread, thread_flavor_t(THREAD_EXTENDED_INFO), intPtr, &count)
    }
  }

  precondition(kr == KERN_SUCCESS, "thread_info failed: \(kr)")
  return extInfo.pth_curpri
}

// Test that escalating a running task actually increases the thread's
// effective priority, not just the task-level priority. Ensure that the
// SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION define isn't accidentally unset.
tests.test("Thread priority escalation") {
  // If we don't have swift_concurrency_private.h then the runtime doesn't have
  // full priority escalation, so don't try to test it.
  guard HasSwiftConcurrencyPrivateHeader() != 0 else { return }

  let readyForEscalation = DispatchSemaphore(value: 0)
  let doneEscalating = DispatchSemaphore(value: 0)
  let doneTesting = DispatchSemaphore(value: 0)

  let task = Task.detached(priority: .background) {
    let priorityBefore = getEffectiveThreadPriority()

    readyForEscalation.signal()
    semaphoreWait(doneEscalating)

    let priorityAfter = getEffectiveThreadPriority()
    let taskPriority = Task.currentPriority

    expectTrue(priorityAfter > priorityBefore,
      "Thread priority should have been escalated from \(priorityBefore), " +
      "but got \(priorityAfter). Task.currentPriority shows \(taskPriority). ")

    doneTesting.signal()
  }

  semaphoreWait(readyForEscalation)
  task.escalatePriority(to: .userInitiated)
  doneEscalating.signal()
  semaphoreWait(doneTesting)
}

await runAllTestsAsync()
