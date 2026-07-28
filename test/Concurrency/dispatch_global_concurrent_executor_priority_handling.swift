// RUN: %target-run-simple-swift(%import-libdispatch -parse-as-library)

// REQUIRES: VENDOR=apple
// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

import Dispatch
import StdlibUnittest

fileprivate func createDispatchQoS() -> DispatchQoS {
  DispatchQoS(
    qosClass: .init(rawValue: qos_class_self())!,
    relativePriority: 0
  )
}

fileprivate func testTaskPriorityMapsToExpectedQoS(
  _ currentPriority: TaskPriority,
  _ currentQoS: DispatchQoS
) {
  if currentPriority > TaskPriority.high {
    expectEqual(currentQoS, DispatchQoS.userInteractive)

  } else if currentPriority > TaskPriority.medium {
    expectEqual(currentQoS, DispatchQoS.userInitiated)

  } else if currentPriority > TaskPriority.low {
    expectEqual(currentQoS, DispatchQoS.default)

  } else if currentPriority > TaskPriority.background {
    expectEqual(currentQoS, DispatchQoS.utility)

  } else {
    expectEqual(currentQoS, DispatchQoS.background)
  }
}

@main
enum App {
  static func main() async {
    let tests = TestSuite("Dispatch global concurrent executor priority handling")

    tests.test("Predefined TaskPriority values") { // Test all non-deprecated TaskPriority values
      let (stream, continuation) = AsyncStream<Void>.makeStream()

      Task.detached(priority: .high) {
        testTaskPriorityMapsToExpectedQoS(
          Task.currentPriority,
          createDispatchQoS()
        )

        continuation.yield()
      }

      Task.detached(priority: .userInitiated) { // `.high` alias
        testTaskPriorityMapsToExpectedQoS(
          Task.currentPriority,
          createDispatchQoS()
        )

        continuation.yield()
      }

      Task.detached(priority: .medium) {
        testTaskPriorityMapsToExpectedQoS(
          Task.currentPriority,
          createDispatchQoS()
        )

        continuation.yield()
      }

      Task.detached(priority: .low) {
        testTaskPriorityMapsToExpectedQoS(
          Task.currentPriority,
          createDispatchQoS()
        )

        continuation.yield()
      }

      Task.detached(priority: .utility) { // `.low` alias
        testTaskPriorityMapsToExpectedQoS(
          Task.currentPriority,
          createDispatchQoS()
        )

        continuation.yield()
      }

      Task.detached(priority: .background) {
        testTaskPriorityMapsToExpectedQoS(
          Task.currentPriority,
          createDispatchQoS()
        )

        continuation.yield()
      }

      var taskCount = 6

      for await _ in stream {
        taskCount -= 1
        if taskCount == 0 { break }
      }
    }

    tests.test("TaskPriority values created from raw values") {
      let (stream, continuation) = AsyncStream<Void>.makeStream()

      Task.detached(priority: .init(rawValue: 255)) {
        testTaskPriorityMapsToExpectedQoS(
          Task.currentPriority,
          createDispatchQoS()
        )

        continuation.yield()
      }

      Task.detached(priority: .init(rawValue: 1)) {
        testTaskPriorityMapsToExpectedQoS(
          Task.currentPriority,
          createDispatchQoS()
        )

        continuation.yield()
      }

      /*
      //rawValue `0` maps to `.default` TaskPriority. Is this expected?
      Task.detached(priority: .init(rawValue: 0)) {
        testTaskPriorityMapsToExpectedQoS(
          Task.currentPriority,
          createDispatchQoS()
        )

        continuation.yield()
      }
      */

      var taskCount = 2

      for await _ in stream {
        taskCount -= 1
        if taskCount == 0 { break }
      }
    }

    await runAllTestsAsync()
  }
}
