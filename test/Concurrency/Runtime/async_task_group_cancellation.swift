// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency) | %FileCheck %s --dump-input always
// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: OS=macosx

import Dispatch

// ==== ------------------------------------------------------------------------
// MARK: "Infrastructure" for the tests

extension DispatchQueue {
  func async<R>(operation: @escaping () async -> R) -> Task.Handle<R> {
    let handle = Task.runDetached(operation: operation)

    // Run the task
    _ = { self.async { handle.run() } }() // force invoking the non-async version

    return handle
  }
}

@available(*, deprecated, message: "This is a temporary hack")
@discardableResult
func launch<R>(operation: @escaping () async -> R) -> Task.Handle<R> {
  let handle = Task.runDetached(operation: operation)

  // Run the task
  _ = DispatchQueue.global(priority: .default).async { handle.run() }

  return handle
}

// ==== ------------------------------------------------------------------------
// MARK: Tests

func test_taskGroup_cancel_internally_afterOne() {
  let taskHandle = launch { () async -> Int in
    return await try! Task.withGroup(resultType: Int.self) { (group) async -> Int in

      func submit(_ n: Int) async {
        await group.add {
          do {
            await try Task.checkCancellation()
            print("submitted: \(n)")
            return n
          } catch {
            print("submit failed: \(error)")
            throw error
          }
        }
      }

      func pull() async -> Result<Int, Error>? {
        do {
          if let r = await try group.next() {
            return .success(r)
          } else {
            return nil
          }
        } catch {
          return .failure(error)
        }
      }

      await submit(1)

      var sum = 0
      while let r = await pull() {
        print("next: \(r)")
        await submit(1)

        if sum >= 2 {
          print("task group returning: \(sum)")
          return sum
        }

        sum += 1
        group.cancelAll() // cancel all after we receive one element
      }

      print("bad, task group returning: \(sum)")
      return 0
    }
  }

  // CHECK: main task
  // CHECK: next: 1
  // CHECK: next: 2
  // CHECK: task group returning: 3

  launch { () async in
    let sum = await try! taskHandle.get()
    // CHECK: result: 2
    print("result: \(sum)")
    exit(0)
  }

  print("main task")
}

test_taskGroup_cancel_internally_afterOne()

dispatchMain()
