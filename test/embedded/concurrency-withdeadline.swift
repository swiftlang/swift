// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library %s -c -o %t/a.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/a.o -o %t/a.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple %target-clang-resource-dir-opt -lswift_Concurrency %target-swift-default-executor-opt %target-embedded-concurrency-threading-shim -dead_strip
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// REQUIRES: OS=macosx || OS=wasip1
// REQUIRES: swift_feature_Embedded

@_spi(Concurrency) import _Concurrency

// Embedded Swift has no ContinuousClock, so the test drives its own clock. Time
// only moves when the test calls `advance(to:)`, which also wakes the timer
// that `withDeadline` has parked in `sleep(until:)`.

struct Tick: InstantProtocol {
  var value: Int

  func advanced(by duration: Duration) -> Tick {
    Tick(value: value + Int(duration.components.seconds))
  }
  func duration(to other: Tick) -> Duration {
    .seconds(other.value - value)
  }
  static func < (lhs: Tick, rhs: Tick) -> Bool { lhs.value < rhs.value }
}

nonisolated(unsafe) var currentTick = 0
nonisolated(unsafe) var parkedTimer: UnsafeContinuation<Void, any Error>? = nil

struct TestClock: Clock, Identifiable {
  typealias Instant = Tick
  typealias Duration = Swift.Duration

  let id: Int

  var now: Tick { Tick(value: currentTick) }
  var minimumResolution: Duration { .seconds(1) }

  func sleep(until deadline: Tick, tolerance: Duration?) async throws {
    try await withTaskCancellationHandler(
      operation: { () async throws -> Void in
        try await withUnsafeThrowingContinuation {
          (continuation: UnsafeContinuation<Void, any Error>) -> Void in
          // The handler runs when the task is cancelled, which can be before this
          // closure parks the continuation. Check for that here.
          if Task.isCancelled {
            continuation.resume(throwing: CancellationError())
          } else if currentTick >= deadline.value {
            continuation.resume(returning: ())
          } else {
            parkedTimer = continuation
          }
        }
      },
      onCancel: {
        if let timer = parkedTimer {
          parkedTimer = nil
          timer.resume(throwing: CancellationError())
        }
      })
  }

  static func advance(to tick: Int) {
    currentTick = tick
    if let timer = parkedTimer {
      parkedTimer = nil
      timer.resume(returning: ())
    }
  }
}

func show(_ label: String, _ value: Tick?) {
  if let value {
    print("\(label): \(value.value)")
  } else {
    print("\(label): none")
  }
}

@main
struct Main {
  static func main() async {
    let clock = TestClock(id: 1)
    let otherClock = TestClock(id: 2)

    // The operation finishes before the deadline.
    print(Task.hasActiveDeadline)
    // CHECK: false
    let result = await withDeadline(Tick(value: 100), clock: clock) { () async -> Int in
      print(Task.hasActiveDeadline)
      show("active", Task.activeDeadline(for: clock))
      show("other clock", Task.activeDeadline(for: otherClock))
      return 42
    }
    print(result)
    // CHECK-NEXT: true
    // CHECK-NEXT: active: 100
    // CHECK-NEXT: other clock: none
    // CHECK-NEXT: 42
    print(Task.hasActiveDeadline)
    show("after", Task.activeDeadline(for: clock))
    // CHECK-NEXT: false
    // CHECK-NEXT: after: none

    // The relative variant adds the timeout to `clock.now`.
    currentTick = 10
    await withDeadline(in: .seconds(5), clock: clock) { () async -> Void in
      show("relative", Task.activeDeadline(for: clock))
    }
    // CHECK-NEXT: relative: 15
    currentTick = 0

    // The deadline expires while the operation runs.
    await withDeadline(Tick(value: 50), clock: clock) { () async -> Void in
      print("cancelled before expiry: \(Task.isCancelled)")
      TestClock.advance(to: 50)
      while !Task.isCancelled {
        await Task.yield()
      }
      print("cancelled after expiry: \(Task.isCancelled)")
    }
    print("outer cancelled: \(Task.isCancelled)")
    // CHECK-NEXT: cancelled before expiry: false
    // CHECK-NEXT: cancelled after expiry: true
    // CHECK-NEXT: outer cancelled: false
    currentTick = 0

    // A deadline in the past cancels the operation before it starts.
    currentTick = 20
    await withDeadline(Tick(value: 10), clock: clock) { () async -> Void in
      print("past deadline cancelled: \(Task.isCancelled)")
    }
    // CHECK-NEXT: past deadline cancelled: true
    currentTick = 0

    // The nearest deadline for a clock wins, and a looser inner deadline
    // leaves the outer one in place.
    await withDeadline(Tick(value: 100), clock: clock) { () async -> Void in
      await withDeadline(Tick(value: 30), clock: clock) { () async -> Void in
        show("tighter", Task.activeDeadline(for: clock))
      }
      await withDeadline(Tick(value: 300), clock: clock) { () async -> Void in
        show("looser", Task.activeDeadline(for: clock))
      }
      show("restored", Task.activeDeadline(for: clock))
    }
    // CHECK-NEXT: tighter: 30
    // CHECK-NEXT: looser: 100
    // CHECK-NEXT: restored: 100

    // Deadlines on different clocks do not see each other.
    await withDeadline(Tick(value: 100), clock: clock) { () async -> Void in
      await withDeadline(Tick(value: 200), clock: otherClock) { () async -> Void in
        show("first clock", Task.activeDeadline(for: clock))
        show("second clock", Task.activeDeadline(for: otherClock))
      }
    }
    // CHECK-NEXT: first clock: 100
    // CHECK-NEXT: second clock: 200

    // Child tasks see the deadline of the task that created them.
    await withDeadline(Tick(value: 100), clock: clock) { () async -> Void in
      async let child = Task.activeDeadline(for: clock)
      show("child", await child)
    }
    // CHECK-NEXT: child: 100

    // The operation can throw, and the error comes out of `withDeadline`.
    struct Failure: Error {}
    do {
      try await withDeadline(Tick(value: 100), clock: clock) { () async throws(Failure) -> Void in
        throw Failure()
      }
    } catch {
      print("caught")
    }
    // CHECK-NEXT: caught
  }
}
