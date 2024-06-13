// RUN: %target-run-simple-swift( -plugin-path %swift-plugin-dir -Xfrontend -disable-availability-checking -parse-stdlib %import-libdispatch)

// REQUIRES: executable_test
// REQUIRES: concurrency

import Swift
import _Concurrency
import Dispatch
import StdlibUnittest

@_silgen_name("swift_task_isCurrentExecutor")
private func isCurrentExecutor(_ executor: Builtin.Executor) -> Bool

private func isCurrentExecutor(_ executor: UnownedSerialExecutor) -> Bool {
    isCurrentExecutor(unsafeBitCast(executor, to: Builtin.Executor.self))
}

extension DispatchGroup {
    func enter(_ count: Int) {
        for _ in 0..<count {
            self.enter()
        }
    }
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
  let group: DispatchGroup
  let probe: Probe

  init(expectedNumber: Int, group: DispatchGroup) {
    self.expectedNumber = expectedNumber
    self.group = group
    self.probe = Probe(expectedNumber: expectedNumber, group: group)
    self.probe.probeExpectedExecutor = self.unownedExecutor
  }

  deinit {
    expectTrue(isCurrentExecutor(self.unownedExecutor))
    expectEqual(expectedNumber, TL.number)
    checkTaskLocalStack()
    group.leave()
  }
}

@globalActor actor AnotherActor: GlobalActor {
  static let shared = AnotherActor()

  func performTesting(_ work: @Sendable () -> Void) {
    work()
  }
}

class Probe {
  var probeExpectedExecutor: UnownedSerialExecutor
  let probeExpectedNumber: Int
  let probeGroup: DispatchGroup
  
  init(expectedNumber: Int, group: DispatchGroup) {
    self.probeExpectedExecutor = AnotherActor.shared.unownedExecutor
    self.probeExpectedNumber = expectedNumber
    self.probeGroup = group
    group.enter()
  }
  
  deinit {
    expectTrue(isCurrentExecutor(probeExpectedExecutor))
    expectEqual(probeExpectedNumber, TL.number)
    checkTaskLocalStack()
    probeGroup.leave()
  }
}

class ClassNoOp: Probe {
  let expectedNumber: Int
  let group: DispatchGroup
  let probe: Probe

  override init(expectedNumber: Int, group: DispatchGroup) {
    self.expectedNumber = expectedNumber
    self.group = group
    self.probe = Probe(expectedNumber: expectedNumber, group: group)
    super.init(expectedNumber: expectedNumber, group: group)
  }

  @AnotherActor
  deinit {
    expectTrue(isCurrentExecutor(AnotherActor.shared.unownedExecutor))
    expectEqual(expectedNumber, TL.number)
    checkTaskLocalStack()
    group.leave()
  }
}

let tests = TestSuite("Isolated Deinit")

if #available(SwiftStdlib 5.1, *) {
  tests.test("fast path") {
    let group = DispatchGroup()
    group.enter(1)
    Task {
      await TL.$number.withValue(42) {
        await AnotherActor.shared.performTesting {
          _ = ClassNoOp(expectedNumber: 42, group: group)
        }
      }
    }
    group.wait()
  }
  
  tests.test("slow path") {
    let group = DispatchGroup()
    group.enter(2)
    Task {
      TL.$number.withValue(37) {
        _ = ActorNoOp(expectedNumber: 0, group: group)
      }
      TL.$number.withValue(99) {
        _ = ClassNoOp(expectedNumber: 0, group: group)
      }
    }
    group.wait()
  }

  tests.test("no TLs") {
    let group = DispatchGroup()
    group.enter(2)
    Task {
      _ = ActorNoOp(expectedNumber: 0, group: group)
      _ = ClassNoOp(expectedNumber: 0, group: group)
    }
    group.wait()
  }
}

runAllTests()

