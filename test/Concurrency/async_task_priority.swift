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

// rdar://101077408 – Temporarily disable on watchOS simulator
// UNSUPPORTED: DARWIN_SIMULATOR=watchos

import Darwin
@preconcurrency import Dispatch
import StdlibUnittest

func loopUntil(priority: TaskPriority) async {
  while (Task.currentPriority != priority) {
    await Task.sleep(1_000_000_000)
  }
}

func print(_ s: String = "") {
  fputs("\(s)\n", stderr)
}

func expectedBasePri(priority: TaskPriority) -> TaskPriority {
  let basePri = Task.basePriority!
  print("Testing basePri matching expected pri - \(basePri) == \(priority)")
  expectEqual(basePri, priority)

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

func childTaskWaitingForEscalation(sem: DispatchSemaphore, basePri: TaskPriority, curPri : TaskPriority) async {
    sem.wait() /* Wait to be escalated */
    let _ = await testNestedTaskPriority(basePri: basePri, curPri: curPri)
}


@main struct Main {
  static func main() async {

    let top_level = detach { /* To detach from main actor when running work */

      let tests = TestSuite("Task Priority manipulations")
      if #available(SwiftStdlib 5.1, *) {

        tests.test("Basic escalation test when task is running") {
            let parentPri = Task.currentPriority

            let sem = DispatchSemaphore(value: 0)
            let task = Task(priority: .background) {
                let _ = expectedBasePri(priority: .background)

                // Wait until task is running before asking to be escalated
                sem.signal()
                sleep(1)

                let _ = expectedEscalatedPri(priority: parentPri)
            }

            // Wait till child runs and asks to be escalated
            sem.wait()
            await task.value
        }

        tests.test("Basic escalation when task is suspended") {
            let parentPri = Task.currentPriority

            let task = Task(priority: .background) {
                await loopUntil(priority: parentPri) /* Suspend task until it is escalated */

                let _ = expectedEscalatedPri(priority: parentPri)
            }
            await task.value // Escalate task BG -> DEF
        }

        tests.test("Structured concurrency priority propagation") {
          let task = Task(priority: .background) {
            await loopUntil(priority: .default)

            let basePri = expectedBasePri(priority: .background)
            let curPri = expectedEscalatedPri(priority: .default)

            // Structured concurrency via async let, escalated priority of
            // parent should propagate
            print("Testing propagation for async let structured concurrency child")
            async let child = testNestedTaskPriority(basePri: basePri, curPri: curPri)
            await child

            let dispatchGroup = DispatchGroup()
            // Structured concurrency via task groups, escalated priority should
            // propagate
            await withTaskGroup(of: Void.self, returning: Void.self) { group in
              dispatchGroup.enter()
              group.addTask {
                print("Testing propagation for task group regular child")
                let _ = await testNestedTaskPriority(basePri: basePri, curPri: curPri)
                dispatchGroup.leave()
                return
              }

              dispatchGroup.enter()
              group.addTask(priority: .utility) {
                print("Testing propagation for task group child with specified priority")
                let _ = await testNestedTaskPriority(basePri: .utility, curPri: curPri)
                dispatchGroup.leave()
                return
              }

              // Wait for child tasks to finish running, don't await since that
              // will escalate them
              dispatchGroup.wait()
            }
          }

          await task.value // Escalate task BG->DEF
        }

        tests.test("Unstructured tasks priority propagation") {
          let task = Task(priority : .background) {
            await loopUntil(priority: .default)

            let basePri = expectedBasePri(priority: .background)
            let _ = expectedEscalatedPri(priority: .default)

            let group = DispatchGroup()

            // Create an unstructured task
            group.enter()
            let _ = Task {
              let _ = await testNestedTaskPriority(basePri: basePri, curPri: basePri)
              group.leave()
              return
            }

            // Wait for unstructured task to finish running, don't await it
            // since that will escalate
            group.wait()
          }

          await task.value // Escalate task BG->DEF
        }

        tests.test("Task escalation propagation to SC children") {
            // Create a task tree and then escalate the parent
            let parentPri = Task.currentPriority
            let basePri : TaskPriority = .background

            let sem = DispatchSemaphore(value: 0)
            let sem2 = DispatchSemaphore(value : 0)

            let task = Task(priority: basePri) {

                async let child = childTaskWaitingForEscalation(sem: sem2, basePri: basePri, curPri: parentPri)

                await withTaskGroup(of: Void.self, returning: Void.self) { group in
                    group.addTask {
                        let _ = await childTaskWaitingForEscalation(sem: sem2, basePri: basePri, curPri: parentPri)
                    }
                    group.addTask(priority: .utility) {
                        let _ = await childTaskWaitingForEscalation(sem: sem2, basePri: .utility, curPri: parentPri)
                    }

                    sem.signal() // Ask for escalation after creating full task tree
                    sleep(1)

                    let _ = expectedBasePri(priority: basePri)
                    let _ = expectedEscalatedPri(priority: parentPri)

                    sem2.signal() // Ask async let child to evaluate
                    sem2.signal() // Ask task group child 1 to evaluate
                    sem2.signal() // Ask task group child 2 to evaluate
                }
            }

            // Wait until children are created and then ask for escalation of
            // top level
            sem.wait()
            await task.value
        }
      }
      await runAllTestsAsync()
    }

    await top_level.value
  }
}
