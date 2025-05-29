// RUN: %target-typecheck-verify-swift -strict-concurrency=complete -disable-availability-checking -parse-as-library
// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking -parse-as-library)

// REQUIRES: concurrency
// REQUIRES: executable_test
// REQUIRES: concurrency_runtime

// Test requires _swift_task_enterThreadLocalContext which is not available 
// in the back deployment runtime.
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: back_deploy_concurrency

import _Concurrency
import StdlibUnittest

@MainActor var tests = TestSuite("Time")

@main struct Main {
  static func main() async {
    tests.test("ContinuousClock sleep")
    .skip(.always("failed despite increased timeout (rdar://94451729)"))
    .code {
      let clock = ContinuousClock()
      let elapsed = await clock.measure {
        try! await clock.sleep(until: .now + .milliseconds(100))
      }
      // give a reasonable range of expected elapsed time
      expectGT(elapsed, .milliseconds(90))
      expectLT(elapsed, .milliseconds(1000))
    }

    tests.test("ContinuousClock sleep with tolerance")
    .skip(.always("failed despite increased timeout (rdar://94451729)"))
    .code {
      let clock = ContinuousClock()
      let elapsed = await clock.measure {
        try! await clock.sleep(until: .now + .milliseconds(100), tolerance: .milliseconds(100))
      }
      // give a reasonable range of expected elapsed time
      expectGT(elapsed, .milliseconds(90))
      expectLT(elapsed, .milliseconds(2000))
    }

    tests.test("ContinuousClock sleep longer")
    .skip(.always("failed despite increased timeout (rdar://94451729)"))
    .code {
      let elapsed = await ContinuousClock().measure {
        try! await Task.sleep(until: .now + .seconds(1), clock: .continuous)
      }
      expectGT(elapsed, .seconds(1) - .milliseconds(90))
      expectLT(elapsed, .seconds(1) + .milliseconds(1000))
    }

    tests.test("SuspendingClock sleep")
    .skip(.always("failed despite increased timeout (rdar://94451729)"))
    .code {
      let clock = SuspendingClock()
      let elapsed = await clock.measure {
        try! await clock.sleep(until: .now + .milliseconds(100))
      }
      // give a reasonable range of expected elapsed time
      expectGT(elapsed, .milliseconds(90))
      expectLT(elapsed, .milliseconds(1000))
    }

    tests.test("SuspendingClock sleep with tolerance")
    .skip(.always("failed despite increased timeout (rdar://94451729)"))
    .code {
      let clock = SuspendingClock()
      let elapsed = await clock.measure {
        try! await clock.sleep(until: .now + .milliseconds(100), tolerance: .milliseconds(100))
      }
      // give a reasonable range of expected elapsed time
      expectGT(elapsed, .milliseconds(90))
      expectLT(elapsed, .milliseconds(2000))
    }

    tests.test("SuspendingClock sleep longer")
    .skip(.always("failed despite increased timeout (rdar://94451729)"))
    .code {
      let elapsed = await SuspendingClock().measure {
        try! await Task.sleep(until: .now + .seconds(1), clock: .suspending)
      }
      expectGT(elapsed, .seconds(1) - .milliseconds(90))
      expectLT(elapsed, .seconds(1) + .milliseconds(1000))
    }

    tests.test("duration addition") {
      let d1 = Duration.milliseconds(500)
      let d2 = Duration.milliseconds(500)
      let d3 = Duration.milliseconds(-500)
      let sum = d1 + d2
      expectEqual(sum, .seconds(1))
      let comps = sum.components
      expectEqual(comps.seconds, 1)
      expectEqual(comps.attoseconds, 0)
      let adjusted = sum + d3
      expectEqual(adjusted, .milliseconds(500))
    }

    tests.test("duration subtraction") {
      let d1 = Duration.nanoseconds(500)
      let d2 = d1 - .nanoseconds(100)
      expectEqual(d2, .nanoseconds(400))
      let d3 = d1 - .nanoseconds(500)
      expectEqual(d3, .nanoseconds(0))
      let d4 = d1 - .nanoseconds(600)
      expectEqual(d4, .nanoseconds(-100))
    }

    tests.test("duration division") {
      let d1 = Duration.seconds(1)
      let halfSecond = d1 / 2
      expectEqual(halfSecond, .milliseconds(500))
    }

    tests.test("duration multiplication") {
      let d1 = Duration.seconds(1)
      let twoSeconds = d1 * 2
      expectEqual(twoSeconds, .seconds(2))
    }

    tests.test("Duration components/whole second increments") {
      for i in 0 ..< 1_000_000 {
        let d = Duration.seconds(i)
        let comps = d.components
        expectEqual(comps.seconds, Int64(i))
        expectEqual(comps.attoseconds, 0)
      }
    }

    tests.test("Duration components/1ms increments") {
      for i in 0 ..< 1_000_000 {
        let d = Duration.milliseconds(i)
        let comps = d.components
        expectEqual(comps.seconds, Int64(i / 1000))
        expectEqual(comps.attoseconds, Int64(i % 1000) * 1_000_000_000_000_000)
      }
    }

    tests.test("Duration components/100µs increments") {
      for i in 0 ..< 1_000_000 {
        let ms = 100 * i
        let d = Duration.microseconds(ms)
        let comps = d.components
        expectEqual(comps.seconds, Int64(ms / 1_000_000))
        expectEqual(comps.attoseconds, Int64(ms % 1_000_000) * 1_000_000_000_000)
      }
    }

    tests.test("Duration components/200ns increments") {
      for i in 0 ..< 1_000_000 {
        let ns = 200 * i
        let d = Duration.nanoseconds(ns)
        let comps = d.components
        expectEqual(comps.seconds, Int64(ns / 1_000_000_000))
        expectEqual(comps.attoseconds, Int64(ns % 1_000_000_000) * 1_000_000_000)
      }
    }

    tests.test("Ensure abi layout size of Instant") {
      // If this test fails it means the ABI of ContinuousClock.Instant has been broken!
      // it MUST be the same laoyut of that of Duration
      expectEqual(MemoryLayout<ContinuousClock.Instant>.size, MemoryLayout<Duration>.size)
      expectEqual(MemoryLayout<SuspendingClock.Instant>.size, MemoryLayout<Duration>.size)
    }

    await runAllTestsAsync()
  }
}
