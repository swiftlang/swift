// RUN: %target-run-simple-swift(%import-libdispatch -parse-as-library)

// REQUIRES: concurrency
// REQUIRES: executable_test

// rdar://106849189 move-only types should be supported in freestanding mode
// UNSUPPORTED: freestanding

// UNSUPPORTED: back_deployment_runtime
// REQUIRES: concurrency_runtime

import StdlibUnittest
@_spi(CustomDefaultExecutors) import _Concurrency

@available(SwiftStdlib 6.2, *)
struct TickingClock: Clock {
  struct Duration: DurationProtocol {
    var ticks: Int

    static func / (_ lhs: Self, _ rhs: Int) -> Self {
      return Duration(ticks: lhs.ticks / rhs)
    }
    static func * (_ lhs: Self, rhs: Int) -> Self {
      return Duration(ticks: lhs.ticks * rhs)
    }
    static func / (_ lhs: Self, _ rhs: Self) -> Double {
      return Double(lhs.ticks) / Double(rhs.ticks)
    }
    static func < (_ lhs: Self, _ rhs: Self) -> Bool {
      return lhs.ticks < rhs.ticks
    }
    static func + (_ lhs: Self, _ rhs: Self) -> Self {
      return Duration(ticks: lhs.ticks + rhs.ticks)
    }
    static func - (_ lhs: Self, _ rhs: Self) -> Self {
      return Duration(ticks: lhs.ticks - rhs.ticks)
    }

    static var zero: Self {
      return Duration(ticks: 0)
    }
  }
  struct Instant: InstantProtocol {
    typealias Duration = TickingClock.Duration
    var ticksFromStart: Int

    func advanced(by duration: Duration) -> Self {
      return Instant(ticksFromStart: self.ticksFromStart + duration.ticks)
    }
    func duration(to other: Self) -> Duration {
      return Duration(ticks: other.ticksFromStart - self.ticksFromStart)
    }

    static func < (_ lhs: Self, _ rhs: Self) -> Bool {
      return lhs.ticksFromStart < rhs.ticksFromStart
    }
  }

  private var _now: Instant
  var now: Instant { return _now }
  var minimumResolution: Duration { return Duration(ticks: 1) }
  var traits: ClockTraits { [.monotonic] }

  init() {
    _now = Instant(ticksFromStart: 0)
  }

  // These are a bit of a lie, since this clock is weird and doesn't
  // actually tell the time; for the purposes of this test, we pretend
  // that the ticks are 20ms.
  func convert(from duration: Duration) -> Swift.Duration? {
    return .seconds(Double(duration.ticks) / 50)
  }

  func convert(from duration: Swift.Duration) -> Duration? {
    let (seconds, attoseconds) = duration.components
    let extraTicks = attoseconds / 20_000_000_000_000_000
    return Duration(ticks: Int(seconds * 50) + Int(extraTicks))
  }

  mutating func tick() {
    _now.ticksFromStart += 1
  }

  func sleep(
    until instant: Instant,
    tolerance: Duration? = nil
  ) async throws {
    // Do nothing
  }
}

@available(SwiftStdlib 6.2, *)
struct TockingClock: Clock {
  struct Duration: DurationProtocol {
    var tocks: Int

    static func / (_ lhs: Self, _ rhs: Int) -> Self {
      return Duration(tocks: lhs.tocks / rhs)
    }
    static func * (_ lhs: Self, rhs: Int) -> Self {
      return Duration(tocks: lhs.tocks * rhs)
    }
    static func / (_ lhs: Self, _ rhs: Self) -> Double {
      return Double(lhs.tocks) / Double(rhs.tocks)
    }
    static func < (_ lhs: Self, _ rhs: Self) -> Bool {
      return lhs.tocks < rhs.tocks
    }
    static func + (_ lhs: Self, _ rhs: Self) -> Self {
      return Duration(tocks: lhs.tocks + rhs.tocks)
    }
    static func - (_ lhs: Self, _ rhs: Self) -> Self {
      return Duration(tocks: lhs.tocks - rhs.tocks)
    }

    static var zero: Self {
      return Duration(tocks: 0)
    }
  }
  struct Instant: InstantProtocol {
    typealias Duration = TockingClock.Duration
    var tocksFromStart: Int

    func advanced(by duration: Duration) -> Self {
      return Instant(tocksFromStart: self.tocksFromStart + duration.tocks)
    }
    func duration(to other: Self) -> Duration {
      return Duration(tocks: other.tocksFromStart - self.tocksFromStart)
    }

    static func < (_ lhs: Self, _ rhs: Self) -> Bool {
      return lhs.tocksFromStart < rhs.tocksFromStart
    }
  }

  private var _now: Instant
  var now: Instant { return _now }
  var minimumResolution: Duration { return Duration(tocks: 1) }
  var traits: ClockTraits { [.monotonic] }

  init() {
    _now = Instant(tocksFromStart: 1000)
  }

  // These are a bit of a lie, since this clock is weird and doesn't
  // actually tell the time; for the purposes of this test, we pretend
  // that the tocks are 10ms.
  func convert(from duration: Duration) -> Swift.Duration? {
    return .seconds(Double(duration.tocks) / 100)
  }

  func convert(from duration: Swift.Duration) -> Duration? {
    let (seconds, attoseconds) = duration.components
    let extraTocks = attoseconds / 10_000_000_000_000_000
    return Duration(tocks: Int(seconds * 100) + Int(extraTocks))
  }

  mutating func tock() {
    _now.tocksFromStart += 1
  }

  func sleep(
    until instant: Instant,
    tolerance: Duration? = nil
  ) async throws {
    // Do nothing
  }
}

@available(SwiftStdlib 6.2, *)
@main struct Main {
  static func main() {
    let tests = TestSuite("clocks")

    var clockA = TickingClock()
    let clockB = TickingClock()

    clockA.tick()
    clockA.tick()
    clockA.tick()

    var clockC = TockingClock()

    clockC.tock()

    tests.test("Convert instants from one clock to another") {
      let nowA = clockA.now
      let nowB = clockB.now

      expectEqual(nowA.ticksFromStart, 3)
      expectEqual(nowB.ticksFromStart, 0)

      let futureA = nowA.advanced(by: TickingClock.Duration(ticks: 23))
      let futureB = nowB.advanced(by: TickingClock.Duration(ticks: 42))

      expectEqual(futureA.ticksFromStart, 26)
      expectEqual(futureB.ticksFromStart, 42)

      let futureAinB = clockB.convert(instant: futureA, from: clockA)!
      let futureBinA = clockA.convert(instant: futureB, from: clockB)!

      expectEqual(futureAinB.ticksFromStart, 23)
      expectEqual(futureBinA.ticksFromStart, 45)

      let futureAinBinA = clockA.convert(instant: futureAinB, from: clockB)!
      let futureBinAinB = clockB.convert(instant: futureBinA, from: clockA)!

      expectEqual(futureAinBinA.ticksFromStart, futureA.ticksFromStart)
      expectEqual(futureBinAinB.ticksFromStart, futureB.ticksFromStart)
    }

    tests.test("Convert instants between clocks with different representations") {
      let nowA = clockA.now
      let nowC = clockC.now

      expectEqual(nowA.ticksFromStart, 3)
      expectEqual(nowC.tocksFromStart, 1001)

      let futureA = nowA.advanced(by: TickingClock.Duration(ticks: 23))
      let futureC = nowC.advanced(by: TockingClock.Duration(tocks: 42))

      expectEqual(futureA.ticksFromStart, 26)
      expectEqual(futureC.tocksFromStart, 1043)

      let futureAinC = clockC.convert(instant: futureA, from: clockA)!
      let futureCinA = clockA.convert(instant: futureC, from: clockC)!

      expectEqual(futureAinC.tocksFromStart, 1047)
      expectEqual(futureCinA.ticksFromStart, 24)

      let futureAinCinA = clockA.convert(instant: futureAinC, from: clockC)!
      let futureCinAinC = clockC.convert(instant: futureCinA, from: clockA)!

      expectEqual(futureAinCinA.ticksFromStart, futureA.ticksFromStart)
      expectEqual(futureCinAinC.tocksFromStart, futureC.tocksFromStart)
    }

    tests.test("Convert instants between continuous and suspending clocks") {
      let continuous = ContinuousClock()
      let suspending = SuspendingClock()

      let nowC = continuous.now
      let nowS = suspending.now

      let futureC = nowC.advanced(by: .seconds(5.3))
      let futureS = nowS.advanced(by: .seconds(4.2))

      let futureCinS = suspending.convert(instant: futureC,
                                          from: .continuous)!
      let futureSinC = continuous.convert(instant: futureS,
                                          from: .suspending)!

      let futureCinSinC = continuous.convert(instant: futureCinS,
                                             from: .suspending)!
      let futureSinCinS = suspending.convert(instant: futureSinC,
                                             from: .continuous)!

      // These clocks may not be exact, so allow differences of up to 50ms
      var delta1 = futureCinSinC - futureC
      var delta2 = futureSinCinS - futureS

      // Duration is not SignedNumeric, so we have to do things this way
      if delta1 < .zero {
        delta1 = .zero - delta1
      }
      if delta2 < .zero {
        delta2 = .zero - delta2
      }

      expectLT(delta1, .milliseconds(50))
      expectLT(delta2, .milliseconds(50))
    }

    runAllTests()
  }
}
