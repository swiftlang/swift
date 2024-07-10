// RUN: %empty-directory(%t)
// RUN: %target-build-swift -Xfrontend -disable-availability-checking %import-libdispatch -parse-as-library %s -o %t/a.out
// RUN: %target-codesign %t/a.out

// RUN: %env-SWIFT_IS_CURRENT_EXECUTOR_LEGACY_MODE_OVERRIDE=swift6 %target-run %t/a.out
// RUN: %env-SWIFT_IS_CURRENT_EXECUTOR_LEGACY_MODE_OVERRIDE=legacy %target-run %t/a.out

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime

// REQUIRES: libdispatch

// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: back_deploy_concurrency
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: freestanding

// Disable this test on simulators
// UNSUPPORTED: DARWIN_SIMULATOR=watchos
// UNSUPPORTED: DARWIN_SIMULATOR=ios
// UNSUPPORTED: DARWIN_SIMULATOR=tvos
// UNSUPPORTED: single_threaded_concurrency

import Dispatch
import StdlibUnittest

#if canImport(Darwin)
import Darwin
#elseif canImport(Glibc)
import Glibc
#elseif canImport(Musl)
import Musl
#elseif canImport(Android)
import Android
#elseif os(WASI)
import WASILibc
#elseif os(Windows)
import CRT
import WinSDK
#endif

@available(SwiftStdlib 6.0, *)
final class NaiveOnMainQueueExecutor: SerialExecutor {
  init() {}

  let mainQueue = DispatchQueue.main

  public func enqueue(_ _job: consuming ExecutorJob) {
    let job = UnownedJob(_job)
    mainQueue.async {
      job.runSynchronously(on: self.asUnownedSerialExecutor())
    }
  }

  public func asUnownedSerialExecutor() -> UnownedSerialExecutor {
    UnownedSerialExecutor(complexEquality: self)
  }

  public func checkIsolated() {
    print("\(Self.self).checkIsolated...")
    dispatchPrecondition(condition: .onQueue(self.mainQueue))
  }
}

actor ActorOnNaiveOnMainQueueExecutor {
  let executor: NaiveOnMainQueueExecutor

  init() {
    self.executor = NaiveOnMainQueueExecutor()
  }

  nonisolated var unownedExecutor: UnownedSerialExecutor {
    self.executor.asUnownedSerialExecutor()
  }

  @MainActor
  func checkPreconditionIsolated() async {
    print("Before preconditionIsolated")
    self.preconditionIsolated()
    print("After preconditionIsolated")
  }
}

@main struct Main {
  static func main() async {
    let tests = TestSuite(#file)

    let varName = "SWIFT_IS_CURRENT_EXECUTOR_LEGACY_MODE_OVERRIDE"
    guard let _mode = getenv(varName) else {
      fatalError("Env variable required by test was not set: \(varName)")
    }
    let mode = String(validatingCString: _mode)!

    if #available(SwiftStdlib 6.0, *) {
      tests.test("test preconditionIsolated in mode: \(mode)") {
        if (mode == "legacy") {
          expectCrashLater()
        } // else, swift6 mode should not crash


        let actor = ActorOnNaiveOnMainQueueExecutor()
        await actor.checkPreconditionIsolated()
      }
    }

    await runAllTestsAsync()
  }
}
