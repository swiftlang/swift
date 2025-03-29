// RUN: %empty-directory(%t)
// RUN: %target-build-swift -plugin-path %swift-plugin-dir -target %target-future-triple -parse-stdlib %import-libdispatch %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %env-SWIFT_IS_CURRENT_EXECUTOR_LEGACY_MODE_OVERRIDE=swift6 %target-run %t/a.out

// REQUIRES: libdispatch
// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

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
    _fixLifetime(self)
  }

  isolated deinit {
    expectTrue(isCurrentExecutor(self.unownedExecutor))
    expectEqual(expectedNumber, TL.number)
    checkTaskLocalStack()
    group.leave()
  }
}

actor ProxyActor: Actor {
  private let impl: ActorNoOp

  init(expectedNumber: Int, group: DispatchGroup) {
    self.impl = ActorNoOp(expectedNumber: expectedNumber, group: group)
  }

  isolated deinit {}

  nonisolated var unownedExecutor: UnownedSerialExecutor {
    return impl.unownedExecutor
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
  tests.test("class sync fast path") {
    let group = DispatchGroup()
    group.enter(1)
    Task {
      // FIXME: isolated deinit should be clearing task locals
      await TL.$number.withValue(42) {
        await AnotherActor.shared.performTesting {
          _ = ClassNoOp(expectedNumber: 0, group: group)
        }
      }
    }
    group.wait()
  }

  tests.test("class sync slow path") {
    let group = DispatchGroup()
    group.enter(1)
    Task {
      TL.$number.withValue(99) {
        _ = ClassNoOp(expectedNumber: 0, group: group)
      }
    }
    group.wait()
  }

  tests.test("actor sync fast path") {
    let group = DispatchGroup()
    group.enter(1)
    Task {
      // FIXME: isolated deinit should be clearing task locals
      TL.$number.withValue(99) {
        // Despite last release happening not on the actor itself,
        // this is still a fast path due to optimisation for deallocating actors.
        _ = ActorNoOp(expectedNumber: 0, group: group)
      }
    }
    group.wait()
  }

  tests.test("actor sync slow path") {
    let group = DispatchGroup()
    group.enter(1)
    Task {
      TL.$number.withValue(99) {
        // Using ProxyActor breaks optimization
        _ = ProxyActor(expectedNumber: 0, group: group)
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

