// RUN: %empty-directory(%t)
// RUN: %target-build-swift -plugin-path %swift-plugin-dir -target %target-swift-5.1-abi-triple -parse-stdlib %s -Xfrontend -concurrency-model=task-to-thread -g -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %env-SWIFT_IS_CURRENT_EXECUTOR_LEGACY_MODE_OVERRIDE=swift6 %target-run %t/a.out

// REQUIRES: freestanding
// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime

@_spi(_TaskToThreadModel) import _Concurrency
import StdlibUnittest
import Darwin
import Swift

@_silgen_name("swift_task_isCurrentExecutor")
private func isCurrentExecutor(_ executor: Builtin.Executor) -> Bool

private func isCurrentExecutor(_ executor: UnownedSerialExecutor) -> Bool {
    isCurrentExecutor(unsafeBitCast(executor, to: Builtin.Executor.self))
}

@available(SwiftStdlib 5.1, *)
struct TL {
  @TaskLocal
  static var number: Int = 0
}

func checkTaskLocalStack() {
  TL.$number.withValue(-999) {
    expectEqual(-999, TL.number)
  }
}

actor ActorNoOp {
  let expectedNumber: Int
  let probe: Probe

  init(expectedNumber: Int) {
    self.expectedNumber = expectedNumber
    self.probe = Probe(expectedNumber: expectedNumber)
    self.probe.probeExpectedExecutor = self.unownedExecutor
  }

  isolated deinit {
    expectTrue(isCurrentExecutor(self.unownedExecutor))
    expectEqual(expectedNumber, TL.number)
    checkTaskLocalStack()
  }
}

class Probe {
  var probeExpectedExecutor: UnownedSerialExecutor!
  let probeExpectedNumber: Int
  
  init(expectedNumber: Int) {
    self.probeExpectedExecutor = nil
    self.probeExpectedNumber = expectedNumber
  }

  deinit {
    expectTrue(isCurrentExecutor(probeExpectedExecutor))
    expectEqual(probeExpectedNumber, TL.number)
    checkTaskLocalStack()
  }
}

let tests = TestSuite("Isolated Deinit")

if #available(SwiftStdlib 5.1, *) {
  tests.test("actor sync not isolated") {
    Task.runInline {
      TL.$number.withValue(99) {
        // Freestanding runtime always executes deinit inline
        _ = ActorNoOp(expectedNumber: 99)
      }
    }
  }

  tests.test("no TLs") {
    Task.runInline {
      _ = ActorNoOp(expectedNumber: 0)
    }
  }
}

runAllTests()
