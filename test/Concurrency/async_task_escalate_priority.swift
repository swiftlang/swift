// RUN: %empty-directory(%t)

// RUN: %target-build-swift %s -Xfrontend -disable-availability-checking -parse-as-library -o %t/async_task_priority
// RUN: %target-codesign %t/async_task_priority
// RUN: %target-run %t/async_task_priority

// REQUIRES: VENDOR=apple
// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

// rdar://76038845
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: back_deploy_concurrency

// rdar://101077408 - Temporarily disable on watchOS & iOS simulator
// UNSUPPORTED: DARWIN_SIMULATOR=watchos
// UNSUPPORTED: DARWIN_SIMULATOR=ios
// UNSUPPORTED: DARWIN_SIMULATOR=tvos

// rdar://107390341 - Temporarily disable for arm64e
// UNSUPPORTED: CPU=arm64e

import Darwin
@preconcurrency import Dispatch
import StdlibUnittest

func loopUntil(priority: TaskPriority) async {
  var loops = 10
  var currentPriority = Task.currentPriority
  while (currentPriority != priority) {
    print("Current priority = \(currentPriority) != \(priority)")
    await Task.sleep(1_000_000)
    currentPriority = Task.currentPriority
    loops -= 1
    if loops < 1 {
      fatalError("Task priority was never: \(priority), over multiple loops")
    }
  }
}

func print(_ s: String = "") {
  fputs("\(s)\n", stderr)
}

func expectedBasePri(priority: TaskPriority) -> TaskPriority {
  let basePri = Task.basePriority!
  print("Testing basePri matching expected pri - \(basePri) == \(priority)")
  expectEqual(basePri, priority)
  withUnsafeCurrentTask { unsafeTask in
    guard let unsafeTask else {
      fatalError("Expected to be able to get current task, but could not!")
    }
    // The UnsafeCurrentTask must return the same value
    expectEqual(basePri, unsafeTask.basePriority)
  }

  return basePri
}

func expectedEscalatedPri(priority: TaskPriority) -> TaskPriority {
  let curPri = Task.currentPriority
  print("Testing escalated matching expected pri - \(curPri) == \(priority)")
  expectEqual(curPri, priority)

  return curPri
}

func testNestedTaskPriority(basePri: TaskPriority, curPri: TaskPriority) async {
  let _ = expectedBasePri(priority: basePri)
  let _ = expectedEscalatedPri(priority: curPri)
}

@main struct Main {
  static func main() async {

    let top_level = Task.detached { /* To detach from main actor when running work */

      let tests = TestSuite("Task Priority escalation")
      if #available(SwiftStdlib 5.1, *) {

        tests.test("Basic task_escalate when task is running") {
          let sem1 = DispatchSemaphore(value: 0)
          let sem2 = DispatchSemaphore(value: 0)
          let task = Task(priority: .background) {
            let _ = expectedBasePri(priority: .background)

            // Wait until task is running before asking to be escalated
            sem1.signal()
            sleep(1)

            await loopUntil(priority: .default)
            sem2.signal()
          }

          // Wait till child runs and asks to be escalated
          sem1.wait()
          Task.escalatePriority(task, to: .default)
          sem2.wait()
        }

        tests.test("Trigger task escalation handler") {
          let sem1 = DispatchSemaphore(value: 0)
          let sem2 = DispatchSemaphore(value: 0)
          let semEscalated = DispatchSemaphore(value: 0)

          let task = Task(priority: .background) {
            let _ = expectedBasePri(priority: .background)

            await withTaskPriorityEscalationHandler {
              print("in withTaskPriorityEscalationHandler, Task.currentPriority = \(Task.currentPriority)")

              // Wait until task is running before asking to be escalated
              sem1.signal()
              sleep(1)

              await loopUntil(priority: .default)
              print("in withTaskPriorityEscalationHandler, after loop, Task.currentPriority = \(Task.currentPriority)")
            } onEscalate: { newPriority in
              print("in onEscalate Task.currentPriority = \(Task.currentPriority)")
              print("in onEscalate newPriority = \(newPriority)")
              precondition(newPriority == .default)
              semEscalated.signal()
            }

            print("Current priority = \(Task.currentPriority)")
            print("after withTaskPriorityEscalationHandler")
            sem2.signal()
          }

          // Wait till child runs and asks to be escalated
          sem1.wait()
          task.cancel() // just checking the records don't stomp onto each other somehow
          Task.escalatePriority(task, to: .default)
          semEscalated.wait()
          sem2.wait()
        }
      }

      await runAllTestsAsync()
    }

    await top_level.value
  }
}
