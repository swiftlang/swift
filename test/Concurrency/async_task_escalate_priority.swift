// RUN: %empty-directory(%t)

// RUN: %target-build-swift %s -Xfrontend -disable-availability-checking -parse-as-library -o %t/async_task_escalate_priority
// RUN: %target-codesign %t/async_task_escalate_priority
// RUN: %target-run %t/async_task_escalate_priority

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

import Darwin
@preconcurrency import Dispatch

func p(_ message: String, file: String = #fileID, line: UInt = #line) {
  print("[:\(line)] \(message)")
}

func expectEqual(_ l: TaskPriority, _ r: TaskPriority,
                 function: String = #function, file: String = #fileID, line: UInt = #line) {
  precondition(l == r, "Priority [\(l)] did not equal \(r) @ \(file):\(line)(\(function))")
}

func loopUntil(priority: TaskPriority, file: String = #fileID, line: UInt = #line) async {
  var loops = 10
  var currentPriority = Task.currentPriority
  while (currentPriority != priority) {
    p("[\(file):\(line)] Current priority = \(currentPriority) != \(priority)")
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
  p("Testing basePri matching expected pri - \(basePri) == \(priority)")
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
  p("Testing escalated matching expected pri - \(curPri) == \(priority)")
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

      func basicTask_escalateWhenTaskIsRunning() {
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
        task.escalatePriority(to: .default)
        sem2.wait()
      }
      basicTask_escalateWhenTaskIsRunning()

      func triggerTaskEscalationHandler() {
        let sem1 = DispatchSemaphore(value: 0)
        let sem2 = DispatchSemaphore(value: 0)
        let semEscalated = DispatchSemaphore(value: 0)

        let task = Task(priority: .background) {
          let _ = expectedBasePri(priority: .background)

          await withTaskPriorityEscalationHandler {
            p("in withTaskPriorityEscalationHandler, Task.currentPriority = \(Task.currentPriority)")

            // Wait until task is running before asking to be escalated
            sem1.signal()
            sleep(1)

            await loopUntil(priority: .default)
            p("in withTaskPriorityEscalationHandler, after loop, Task.currentPriority = \(Task.currentPriority)")
          } onPriorityEscalated: { oldPriority, newPriority in
            p("in onPriorityEscalated Task.currentPriority = \(Task.currentPriority)")
            p("in onPriorityEscalated oldPriority = \(oldPriority)")
            precondition(oldPriority == .background, "old Priority was: \(oldPriority)")
            p("in onPriorityEscalated newPriority = \(newPriority)")
            precondition(newPriority == .default, "new Priority was: \(newPriority)")
            semEscalated.signal()
          }

          p("Current priority = \(Task.currentPriority)")
          p("after withTaskPriorityEscalationHandler")
          sem2.signal()
        }

        // Wait till child runs and asks to be escalated
        sem1.wait()
        task.cancel() // just checking the records don't stomp onto each other somehow
        task.escalatePriority(to: .default)
        semEscalated.wait()
        sem2.wait()
      }
      triggerTaskEscalationHandler()

      func triggerTwice_escalateToMediumAndThenAgainToHigh() {
        let sem1 = DispatchSemaphore(value: 0)
        let semEscalatedMedium = DispatchSemaphore(value: 0)
        let semEscalatedHigh = DispatchSemaphore(value: 0)
        let semEscalatedInHandler = DispatchSemaphore(value: 2)

        let task = Task(priority: .background) {
          let _ = expectedBasePri(priority: .background)

          await withTaskPriorityEscalationHandler {
            p("in withTaskPriorityEscalationHandler, Task.currentPriority = \(Task.currentPriority)")
            sem1.signal()

            p("in withTaskPriorityEscalationHandler, wait for escalation -> \(TaskPriority.medium)")
            await loopUntil(priority: .medium)
            p("in withTaskPriorityEscalationHandler, after loop, Task.currentPriority = \(Task.currentPriority)")
            semEscalatedMedium.signal()

            p("in withTaskPriorityEscalationHandler, wait for escalation -> \(TaskPriority.high)")
            await loopUntil(priority: .high)
            p("in withTaskPriorityEscalationHandler, after loop, Task.currentPriority = \(Task.currentPriority)")
            semEscalatedHigh.signal()
          } onPriorityEscalated: { oldPriority, newPriority in
            p("in onPriorityEscalated Task.currentPriority = \(Task.currentPriority)") // caller priority
            p("in onPriorityEscalated oldPriority = \(oldPriority)")
            p("in onPriorityEscalated newPriority = \(newPriority)")
            semEscalatedInHandler.signal()
          }
          p("after withTaskPriorityEscalationHandler")
        }

        // Wait till child runs and asks to be escalated
        sem1.wait()
        task.escalatePriority(to: .medium)
        semEscalatedMedium.wait()

        task.escalatePriority(to: .high)
        semEscalatedHigh.wait()

        // we got escalated twice
        semEscalatedInHandler.wait()
      }
      triggerTwice_escalateToMediumAndThenAgainToHigh()

      func dontTriggerInAlreadyEscalatedTask() {
        let sem1 = DispatchSemaphore(value: 0)
        let sem2 = DispatchSemaphore(value: 0)
        let semEscalated = DispatchSemaphore(value: 0)

        let task = Task(priority: .background) {
          let _ = expectedBasePri(priority: .background)

          await withTaskPriorityEscalationHandler {
            p("in withTaskPriorityEscalationHandler, Task.currentPriority = \(Task.currentPriority)")
            sem1.signal()
            await loopUntil(priority: .default)
            p("in withTaskPriorityEscalationHandler, after loop, Task.currentPriority = \(Task.currentPriority)")
          } onPriorityEscalated: { oldPriority, newPriority in
            p("in onPriorityEscalated Task.currentPriority = \(Task.currentPriority)")
            p("in onPriorityEscalated oldPriority = \(oldPriority)")
            precondition(oldPriority == .background, "old Priority was: \(oldPriority)")
            p("in onPriorityEscalated newPriority = \(newPriority)")
            precondition(newPriority == .default, "new Priority was: \(newPriority)")
            semEscalated.signal()
          }

          // already escalated
          await loopUntil(priority: .default)

          await withTaskPriorityEscalationHandler {
            await loopUntil(priority: .default)
            p("in withTaskPriorityEscalationHandler, after loop, Task.currentPriority = \(Task.currentPriority)")
          } onPriorityEscalated: { oldPriority, newPriority in
            fatalError("Task was already escalated earlier, no escalation triggered here")
          }

          p("Current priority = \(Task.currentPriority)")
          p("after withTaskPriorityEscalationHandler")
          sem2.signal()
        }

        // Wait till child runs and asks to be escalated
        sem1.wait()
        task.cancel() // just checking the records don't stomp onto each other somehow
        task.escalatePriority(to: .default)
        semEscalated.wait()
        sem2.wait()
      }
      dontTriggerInAlreadyEscalatedTask()
    }

    await top_level.value
  }
}
