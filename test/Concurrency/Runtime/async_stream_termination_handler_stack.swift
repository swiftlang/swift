// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking -parse-as-library)

// REQUIRES: concurrency
// REQUIRES: executable_test
// REQUIRES: concurrency_runtime

// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: freestanding
// XFAIL: OS=emscripten

// Reading `Continuation.onTermination` and writing it back
// must not accumulate closure wrappers. Doing so may create a long chain
// of closures which would be destroyed linearily upon destruction,
// and a too long chain may exhaust the stack.
//
// This is a specific regression test for this situation.
// See: https://github.com/swiftlang/swift/pull/91086 / rdar://183369172
//
// This test uses a small probe in the closure, to see if we're accumulating
// increasingly more closures or not by accident. This would have shown increasing
// "stack usage" in a closure wrapping impl that would be prone to these issues.

import _Concurrency
import Synchronization
import StdlibUnittest

// Releasing the chain must not deepen as round trips accumulate. A regressed
// implementation grows this by roughly 1730 bytes per round trip, so even a
// handful of extra round trips clears this comfortably
let growthToleranceBytes = 4 * 1024

struct ProbeState {
  var releaseAddress: UInt = 0
  var releaseCount: Int = 0
}

let probeState = Mutex(ProbeState())

@inline(never)
func stackAddress() -> UInt {
  var local: Int = 0 // sneaky trick to get stack address we're at
  return withUnsafeMutablePointer(to: &local) { UInt(bitPattern: $0) }
}

// Owned only by the `onTermination` closure, so `deinit` runs at the deepest
// point of the recursive release of the handler chain
final class StackProbe {
  deinit {
    let address = stackAddress()
    probeState.withLock { state in
      state.releaseCount += 1
      state.releaseAddress = address
    }
  }
}

func resetProbe() {
  probeState.withLock { state in
    state = ProbeState()
  }
}

// The returned closure is the sole owner of the probe. Callers must assign it
// straight into `onTermination` without binding it to a local, otherwise the
// enclosing frame keeps the probe alive past the teardown being measured.
@inline(never)
func makeProbeHandler<Termination>(
  _: Termination.Type = Termination.self
) -> @Sendable (Termination) -> Void {
  return { [probe = StackProbe()] _ in
    withExtendedLifetime(probe) {}
  }
}

func stackDelta(from reference: UInt, to measured: UInt) -> Int {
  guard measured != 0 else { return -1 }
  return Int(reference > measured ? reference - measured : measured - reference)
}

// This is synchronous, so `finish()` releases the handler chain on the same
// thread that took the reference address and the two are comparable
@inline(never)
func releaseCost(roundTrips: Int) -> Int {
  let reference = stackAddress()
  resetProbe()
  do {
    let (stream, continuation) = AsyncStream<Int>.makeStream()
    continuation.onTermination = makeProbeHandler()
    for _ in 0..<roundTrips {
      continuation.onTermination = continuation.onTermination
    }
    continuation.finish()
    withExtendedLifetime(stream) {}
  }
  return stackDelta(from: reference, to: probeState.withLock { $0.releaseAddress })
}

@inline(never)
func releaseCostThrowing(roundTrips: Int) -> Int {
  let reference = stackAddress()
  resetProbe()
  do {
    let (stream, continuation) = AsyncThrowingStream<Int, Error>.makeStream()
    continuation.onTermination = makeProbeHandler()
    for _ in 0..<roundTrips {
      continuation.onTermination = continuation.onTermination
    }
    continuation.finish()
    withExtendedLifetime(stream) {}
  }
  return stackDelta(from: reference, to: probeState.withLock { $0.releaseAddress })
}

@MainActor var tests = TestSuite("AsyncStreamTerminationHandlerStack")

@main struct Main {
  static func main() async {
    tests.test("AsyncStream onTermination round trip does not grow the handler chain") {
      let one = releaseCost(roundTrips: 1)
      let many = releaseCost(roundTrips: 8)
      print("release: \(one) bytes at 1 round trip, \(many) bytes at 8")

      expectEqual(1, probeState.withLock { $0.releaseCount })
      expectGE(one, 0)
      expectGE(many, 0)
      expectLT(many - one, growthToleranceBytes)
    }

    tests.test("AsyncThrowingStream onTermination round trip does not grow the handler chain") {
      let one = releaseCostThrowing(roundTrips: 1)
      let many = releaseCostThrowing(roundTrips: 8)
      print("release: \(one) bytes at 1 round trip, \(many) bytes at 8")

      expectEqual(1, probeState.withLock { $0.releaseCount })
      expectGE(one, 0)
      expectGE(many, 0)
      expectLT(many - one, growthToleranceBytes)
    }

    await runAllTestsAsync()
  }
}
