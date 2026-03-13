// RUN: %empty-directory(%t)
//
// RUN: %target-build-swift -module-name a %s -o %t.out -O
// RUN: %target-codesign %t.out
// RUN: %target-run %t.out
// REQUIRES: executable_test
// REQUIRES: stress_test
// UNSUPPORTED: threading_none

import SwiftPrivate
import StdlibUnittest
#if canImport(Darwin)
  import Darwin
#elseif canImport(Glibc)
  import Glibc
#elseif os(Windows)
  import MSVCRT
#else
#error("Unsupported platform")
#endif

final class HeapBool {
  var value: Bool
  init(_ value: Bool) {
    self.value = value
  }
}

final class HeapInt {
  var value: Int
  init(_ value: Int) {
    self.value = value
  }
}

final class AtomicInt4RaceData {
  var writerStarted = _stdlib_AtomicInt(0)
  var a1: _stdlib_AtomicInt
  var a2: _stdlib_AtomicInt
  var a3: _stdlib_AtomicInt
  var a4: _stdlib_AtomicInt

  init(_ a1: Int, _ a2: Int, _ a3: Int, _ a4: Int) {
    self.a1 = _stdlib_AtomicInt(a1)
    self.a2 = _stdlib_AtomicInt(a2)
    self.a3 = _stdlib_AtomicInt(a3)
    self.a4 = _stdlib_AtomicInt(a4)
  }
}

final class AtomicInt4HeapInt2Int2RaceData {
  var writerStarted = _stdlib_AtomicInt(0)
  var a1: _stdlib_AtomicInt
  var a2: _stdlib_AtomicInt
  var a3: _stdlib_AtomicInt
  var a4: _stdlib_AtomicInt
  var hi1: HeapInt
  var hi2: HeapInt
  var i1: Int
  var i2: Int

  init(
    _ a1: Int, _ a2: Int, _ a3: Int, _ a4: Int,
    _ hi1: Int, _ hi2: Int, _ i1: Int, _ i2: Int
  ) {
    self.a1 = _stdlib_AtomicInt(a1)
    self.a2 = _stdlib_AtomicInt(a2)
    self.a3 = _stdlib_AtomicInt(a3)
    self.a4 = _stdlib_AtomicInt(a4)
    self.hi1 = HeapInt(hi1)
    self.hi2 = HeapInt(hi2)
    self.i1 = i1
    self.i2 = i2
  }
}

struct AtomicInt_fetchAndAdd_1_RaceTest : RaceTestWithPerTrialData {
  typealias RaceData = AtomicInt4RaceData
  typealias ThreadLocalData = Void
  typealias Observation = Observation5Int

  func makeRaceData() -> RaceData {
    return RaceData(0, 0, 0, 0)
  }

  func makeThreadLocalData() -> Void {
    return Void()
  }

  func thread1(
    _ raceData: RaceData, _ threadLocalData: inout ThreadLocalData
  ) -> Observation {
    if raceData.writerStarted.fetchAndAdd(1) == 0 {
      // Writer.
      consumeCPU(units: 256)
      let a1 = raceData.a1.fetchAndAdd(10)
      consumeCPU(units: 256)
      let a2 = raceData.a2.fetchAndAdd(20)
      consumeCPU(units: 256)
      let a3 = raceData.a3.addAndFetch(30)
      consumeCPU(units: 256)
      let a4 = raceData.a4.addAndFetch(40)
      return Observation(1, a1, a2, a3, a4)
    } else {
      // Reader.
      //
      // Observing a non-zero a1 does not impose any constraints onto
      // subsequent loads of a2, a3 or a4: since stores to a2, a3 and a4 don't
      // happen before the store to a1, there are executions where we observed
      // a non-zero a1, but storing to a2 does not happen before loading from
      // a2 (same for a3 and a4).
      consumeCPU(units: 256)
      let a1 = raceData.a1.load()
      consumeCPU(units: 256)
      let a2 = raceData.a2.load()
      consumeCPU(units: 256)
      let a3 = raceData.a3.load()
      consumeCPU(units: 256)
      let a4 = raceData.a4.load()

      return Observation(2, a1, a2, a3, a4)
    }
  }

  func evaluateObservations(
    _ observations: [Observation],
    _ sink: (RaceTestObservationEvaluation) -> Void
  ) {
    for observation in observations {
      switch observation {
      case Observation(1,  0,  0, 30, 40),
           Observation(2,  0,  0,  0,  0),
           Observation(2,  0,  0,  0, 40),
           Observation(2,  0,  0, 30,  0),
           Observation(2,  0,  0, 30, 40),
           Observation(2,  0, 20,  0,  0),
           Observation(2,  0, 20,  0, 40),
           Observation(2,  0, 20, 30,  0),
           Observation(2,  0, 20, 30, 40),
           Observation(2, 10,  0,  0,  0),
           Observation(2, 10,  0,  0, 40),
           Observation(2, 10,  0, 30,  0),
           Observation(2, 10,  0, 30, 40),
           Observation(2, 10, 20,  0,  0),
           Observation(2, 10, 20,  0, 40),
           Observation(2, 10, 20, 30,  0),
           Observation(2, 10, 20, 30, 40):
        sink(.passInteresting(String(describing: observation)))

      default:
        sink(.failureInteresting(String(describing: observation)))
      }
    }
  }
}

struct AtomicInt_fetchAndAdd_ReleaseAtomicStores_1_RaceTest
  : RaceTestWithPerTrialData {

  typealias RaceData = AtomicInt4RaceData
  typealias ThreadLocalData = Void
  typealias Observation = Observation5Int

  func makeRaceData() -> RaceData {
    return RaceData(0, 0, 0, 0)
  }

  func makeThreadLocalData() -> Void {
    return Void()
  }

  func thread1(
    _ raceData: RaceData, _ threadLocalData: inout ThreadLocalData
  ) -> Observation {
    // Test release semantics of 'fetchAndAdd' and 'addAndFetch'.
    if raceData.writerStarted.fetchAndAdd(1) == 0 {
      // Writer.
      consumeCPU(units: 256)
      let a1 = raceData.a1.fetchAndAdd(10)
      consumeCPU(units: 256)
      let a2 = raceData.a2.fetchAndAdd(20)
      consumeCPU(units: 256)
      let a3 = raceData.a3.addAndFetch(30)
      consumeCPU(units: 256)
      let a4 = raceData.a4.addAndFetch(40)
      return Observation(1, a1, a2, a3, a4)
    } else {
      // Reader.
      //
      // Observing a non-zero a4 requires observing all other stores as
      // well, since stores to a1, a2 and a3 happen before the store to a4.
      // Same applies to the load from a3 and stores to a1, a2.
      // Same applies to the load from a2 and the store to a1.
      consumeCPU(units: 256)
      let a4 = raceData.a4.load()
      consumeCPU(units: 256)
      let a3 = raceData.a3.load()
      consumeCPU(units: 256)
      let a2 = raceData.a2.load()
      consumeCPU(units: 256)
      let a1 = raceData.a1.load()

      return Observation(2, a1, a2, a3, a4)
    }
  }

  func evaluateObservations(
    _ observations: [Observation],
    _ sink: (RaceTestObservationEvaluation) -> Void
  ) {
    for observation in observations {
      switch observation {
      case Observation(1,  0,  0, 30, 40),
           Observation(2,  0,  0,  0,  0),
           Observation(2, 10,  0,  0,  0),
           Observation(2, 10, 20,  0,  0),
           Observation(2, 10, 20, 30,  0),
           Observation(2, 10, 20, 30, 40):
        sink(.passInteresting(String(describing: observation)))

      default:
        sink(.failureInteresting(String(describing: observation)))
      }
    }
  }
}

struct AtomicInt_fetchAndAdd_ReleaseAtomicStores_2_RaceTest
  : RaceTestWithPerTrialData {

  typealias RaceData = AtomicInt4RaceData
  typealias ThreadLocalData = Void
  typealias Observation = Observation5Int

  func makeRaceData() -> RaceData {
    return RaceData(0, 0, 0, 0)
  }

  func makeThreadLocalData() -> Void {
    return Void()
  }

  func thread1(
    _ raceData: RaceData, _ threadLocalData: inout ThreadLocalData
  ) -> Observation {
    // Test release semantics of 'fetchAndAdd' and 'addAndFetch'.
    if raceData.writerStarted.fetchAndAdd(1) == 0 {
      // Writer.
      consumeCPU(units: 256)
      let a1 = raceData.a1.fetchAndAdd(10)
      consumeCPU(units: 256)
      let a2 = raceData.a2.fetchAndAdd(20)
      consumeCPU(units: 256)
      let a3 = raceData.a3.addAndFetch(30)
      consumeCPU(units: 256)
      let a4 = raceData.a4.addAndFetch(40)
      return Observation(1, a1, a2, a3, a4)
    } else {
      // Reader.
      //
      // Observing a non-zero a4 requires observing all other stores as well,
      // since stores to a1, a2 and a3 happen before the store to a4.  In this
      // test we deliberately load a1 before a2 so that the load from a2 does
      // not impose additional ordering on a1 (the happens before relation
      // between load and store to a4 should be sufficient.)  We also
      // deliberately load all non-atomic properties first so that additional
      // atomic loads don't impose additional ordering on them.
      consumeCPU(units: 256)
      let a4 = raceData.a4.load()
      consumeCPU(units: 256)
      if a4 != 0 {
        let a1 = raceData.a1.load()
        consumeCPU(units: 256)
        let a2 = raceData.a2.load()
        consumeCPU(units: 256)
        let a3 = raceData.a3.load()
        return Observation(2, a1, a2, a3, a4)
      }
      let a3 = raceData.a3.load()
      consumeCPU(units: 256)
      if a3 != 0 {
        let a1 = raceData.a1.load()
        consumeCPU(units: 256)
        let a2 = raceData.a2.load()
        return Observation(2, a1, a2, a3, a4)
      }
      let a2 = raceData.a2.load()
      consumeCPU(units: 256)
      let a1 = raceData.a1.load()
      return Observation(2, a1, a2, a3, a4)
    }
  }

  func evaluateObservations(
    _ observations: [Observation],
    _ sink: (RaceTestObservationEvaluation) -> Void
  ) {
    for observation in observations {
      switch observation {
      case Observation(1,  0,  0, 30, 40),
           Observation(2,  0,  0,  0,  0),
           Observation(2, 10,  0,  0,  0),
           Observation(2, 10, 20,  0,  0),
           Observation(2, 10, 20, 30,  0),
           Observation(2, 10, 20, 30, 40):
        sink(.passInteresting(String(describing: observation)))

      default:
        sink(.failureInteresting(String(describing: observation)))
      }
    }
  }
}

struct AtomicInt_fetchAndAdd_ReleaseNonAtomicStores_RaceTest
  : RaceTestWithPerTrialData {

  typealias RaceData = AtomicInt4HeapInt2Int2RaceData
  typealias ThreadLocalData = Void
  typealias Observation = Observation9Int

  func makeRaceData() -> RaceData {
    return RaceData(0, 0, 0, 0, 0, 0, 0, 0)
  }

  func makeThreadLocalData() -> Void {
    return Void()
  }

  func thread1(
    _ raceData: RaceData, _ threadLocalData: inout ThreadLocalData
  ) -> Observation {
    // Test release semantics of 'fetchAndAdd' and 'addAndFetch' with regard to
    // non-atomic stores.
    if raceData.writerStarted.fetchAndAdd(1) == 0 {
      // Writer.
      consumeCPU(units: 256)
      raceData.hi1.value = 100
      consumeCPU(units: 256)
      let a1 = raceData.a1.fetchAndAdd(10)
      consumeCPU(units: 256)
      raceData.hi2.value = 200
      consumeCPU(units: 256)
      let a2 = raceData.a2.fetchAndAdd(20)
      consumeCPU(units: 256)
      raceData.i1 = 300
      consumeCPU(units: 256)
      let a3 = raceData.a3.addAndFetch(30)
      consumeCPU(units: 256)
      raceData.i2 = 400
      consumeCPU(units: 256)
      let a4 = raceData.a4.addAndFetch(40)
      return Observation(
        1, a1, a2, a3, a4,
        raceData.hi1.value, raceData.hi2.value,
        raceData.i1, raceData.i2)
    } else {
      // Reader.
      //
      // Observing a non-zero a4 requires observing all other stores as well,
      // since stores to a1, a2 and a3 happen before the store to a4.  In this
      // test we deliberately load a1 before a2 so that the load from a2 does
      // not imply additional ordering on a1 (the happens before relation
      // between load and store to a4 should be sufficient.)
      consumeCPU(units: 256)
      let a4 = raceData.a4.load()
      consumeCPU(units: 256)
      if a4 != 0 {
        let hi1 = raceData.hi1.value
        consumeCPU(units: 256)
        let hi2 = raceData.hi2.value
        consumeCPU(units: 256)
        let i1 = raceData.i1
        consumeCPU(units: 256)
        let i2 = raceData.i2
        consumeCPU(units: 256)
        let a1 = raceData.a1.load()
        consumeCPU(units: 256)
        let a2 = raceData.a2.load()
        consumeCPU(units: 256)
        let a3 = raceData.a3.load()
        return Observation(2, a1, a2, a3, a4, hi1, hi2, i1, i2)
      }
      let i2 = 999 // Loading i2 would cause a race.
      let a3 = raceData.a3.load()
      consumeCPU(units: 256)
      if a3 != 0 {
        let hi1 = raceData.hi1.value
        consumeCPU(units: 256)
        let hi2 = raceData.hi2.value
        consumeCPU(units: 256)
        let i1 = raceData.i1
        consumeCPU(units: 256)
        let a1 = raceData.a1.load()
        consumeCPU(units: 256)
        let a2 = raceData.a2.load()
        return Observation(2, a1, a2, a3, a4, hi1, hi2, i1, i2)
      }
      let i1 = 999 // Loading i1 would cause a race.
      let a2 = raceData.a2.load()
      consumeCPU(units: 256)
      if a2 != 0 {
        let hi1 = raceData.hi1.value
        consumeCPU(units: 256)
        let hi2 = raceData.hi2.value
        consumeCPU(units: 256)
        let a1 = raceData.a1.load()
        return Observation(2, a1, a2, a3, a4, hi1, hi2, i1, i2)
      }
      let hi2 = 999 // Loading hi2 would cause a race.
      let a1 = raceData.a1.load()
      if a1 != 0 {
        let hi1 = raceData.hi1.value
        return Observation(2, a1, a2, a3, a4, hi1, hi2, i1, i2)
      }
      let hi1 = 999 // Loading hi2 would cause a race.
      return Observation(2, a1, a2, a3, a4, hi1, hi2, i1, i2)
    }
  }

  func evaluateObservations(
    _ observations: [Observation],
    _ sink: (RaceTestObservationEvaluation) -> Void
  ) {
    for observation in observations {
      switch observation {
      case Observation(1,  0,  0, 30, 40, 100, 200, 300, 400),
           Observation(2,  0,  0,  0,  0, 999, 999, 999, 999),
           Observation(2, 10,  0,  0,  0, 100, 999, 999, 999),
           Observation(2, 10, 20,  0,  0, 100, 200, 999, 999),
           Observation(2, 10, 20, 30,  0, 100, 200, 300, 999),
           Observation(2, 10, 20, 30, 40, 100, 200, 300, 400):
        sink(.passInteresting(String(describing: observation)))

      default:
        sink(.failureInteresting(String(describing: observation)))
      }
    }
  }
}

struct AtomicInt_fetchAndAnd_1_RaceTest : RaceTestWithPerTrialData {
  typealias RaceData = AtomicInt4RaceData
  typealias ThreadLocalData = Void
  typealias Observation = Observation5Int

  func makeRaceData() -> RaceData {
    let start = ~(1 + 2) // Set all bits except two.
    return RaceData(start, start, start, start)
  }

  func makeThreadLocalData() -> Void {
    return Void()
  }

  func thread1(
    _ raceData: RaceData, _ threadLocalData: inout ThreadLocalData
  ) -> Observation {
    if raceData.writerStarted.fetchAndAdd(1) == 0 {
      // Writer.
      consumeCPU(units: 256)
      let a1 = raceData.a1.fetchAndAnd(1 + 8)
      consumeCPU(units: 256)
      let a2 = raceData.a2.fetchAndAnd(1 + 16)
      consumeCPU(units: 256)
      let a3 = raceData.a3.andAndFetch(1 + 32)
      consumeCPU(units: 256)
      let a4 = raceData.a4.andAndFetch(1 + 64)
      return Observation(1, a1, a2, a3, a4)
    } else {
      // Reader.
      //
      // Observing a non-zero a1 does not impose any constraints onto
      // subsequent loads of a2, a3 or a4: since stores to a2, a3 and a4 don't
      // happen before the store to a1, there are executions where we observed
      // a non-zero a1, but storing to a2 does not happen before loading from
      // a2 (same for a3 and a4).
      consumeCPU(units: 256)
      let a1 = raceData.a1.load()
      consumeCPU(units: 256)
      let a2 = raceData.a2.load()
      consumeCPU(units: 256)
      let a3 = raceData.a3.load()
      consumeCPU(units: 256)
      let a4 = raceData.a4.load()

      return Observation(2, a1, a2, a3, a4)
    }
  }

  func evaluateObservations(
    _ observations: [Observation],
    _ sink: (RaceTestObservationEvaluation) -> Void
  ) {
    for observation in observations {
      switch observation {
      case Observation(1, -4, -4, 32, 64),
           Observation(2, -4, -4, -4, -4),
           Observation(2, -4, -4, -4, 64),
           Observation(2, -4, -4, 32, -4),
           Observation(2, -4, -4, 32, 64),
           Observation(2, -4, 16, -4, -4),
           Observation(2, -4, 16, -4, 64),
           Observation(2, -4, 16, 32, -4),
           Observation(2, -4, 16, 32, 64),
           Observation(2,  8, -4, -4, -4),
           Observation(2,  8, -4, -4, 64),
           Observation(2,  8, -4, 32, -4),
           Observation(2,  8, -4, 32, 64),
           Observation(2,  8, 16, -4, -4),
           Observation(2,  8, 16, -4, 64),
           Observation(2,  8, 16, 32, -4),
           Observation(2,  8, 16, 32, 64):
        sink(.passInteresting(String(describing: observation)))

      default:
        sink(.failureInteresting(String(describing: observation)))
      }
    }
  }
}

struct AtomicInt_fetchAndOr_1_RaceTest : RaceTestWithPerTrialData {
  typealias RaceData = AtomicInt4RaceData
  typealias ThreadLocalData = Void
  typealias Observation = Observation5Int

  func makeRaceData() -> RaceData {
    let start = 1 + 2 // Set two bits.
    return RaceData(start, start, start, start)
  }

  func makeThreadLocalData() -> Void {
    return Void()
  }

  func thread1(
    _ raceData: RaceData, _ threadLocalData: inout ThreadLocalData
  ) -> Observation {
    if raceData.writerStarted.fetchAndAdd(1) == 0 {
      // Writer.
      consumeCPU(units: 256)
      let a1 = raceData.a1.fetchAndOr(1 + 8)
      consumeCPU(units: 256)
      let a2 = raceData.a2.fetchAndOr(1 + 16)
      consumeCPU(units: 256)
      let a3 = raceData.a3.orAndFetch(1 + 32)
      consumeCPU(units: 256)
      let a4 = raceData.a4.orAndFetch(1 + 64)
      return Observation(1, a1, a2, a3, a4)
    } else {
      // Reader.
      //
      // Observing a non-zero a1 does not impose any constraints onto
      // subsequent loads of a2, a3 or a4: since stores to a2, a3 and a4 don't
      // happen before the store to a1, there are executions where we observed
      // a non-zero a1, but storing to a2 does not happen before loading from
      // a2 (same for a3 and a4).
      consumeCPU(units: 256)
      let a1 = raceData.a1.load()
      consumeCPU(units: 256)
      let a2 = raceData.a2.load()
      consumeCPU(units: 256)
      let a3 = raceData.a3.load()
      consumeCPU(units: 256)
      let a4 = raceData.a4.load()

      return Observation(2, a1, a2, a3, a4)
    }
  }

  func evaluateObservations(
    _ observations: [Observation],
    _ sink: (RaceTestObservationEvaluation) -> Void
  ) {
    for observation in observations {
      switch observation {
      case Observation(1,  3,  3, 35, 67),
           Observation(2,  3,  3,  3,  3),
           Observation(2,  3,  3,  3, 67),
           Observation(2,  3,  3, 35,  3),
           Observation(2,  3,  3, 35, 67),
           Observation(2,  3, 19,  3,  3),
           Observation(2,  3, 19,  3, 67),
           Observation(2,  3, 19, 35,  3),
           Observation(2,  3, 19, 35, 67),
           Observation(2, 11,  3,  3,  3),
           Observation(2, 11,  3,  3, 67),
           Observation(2, 11,  3, 35,  3),
           Observation(2, 11,  3, 35, 67),
           Observation(2, 11, 19,  3,  3),
           Observation(2, 11, 19,  3, 67),
           Observation(2, 11, 19, 35,  3),
           Observation(2, 11, 19, 35, 67):
        sink(.passInteresting(String(describing: observation)))

      default:
        sink(.failureInteresting(String(describing: observation)))
      }
    }
  }
}

struct AtomicInt_fetchAndXor_1_RaceTest : RaceTestWithPerTrialData {
  typealias RaceData = AtomicInt4RaceData
  typealias ThreadLocalData = Void
  typealias Observation = Observation5Int

  func makeRaceData() -> RaceData {
    let start = 1 + 2 // Set two bits.
    return RaceData(start, start, start, start)
  }

  func makeThreadLocalData() -> Void {
    return Void()
  }

  func thread1(
    _ raceData: RaceData, _ threadLocalData: inout ThreadLocalData
  ) -> Observation {
    if raceData.writerStarted.fetchAndAdd(1) == 0 {
      // Writer.
      consumeCPU(units: 256)
      let a1 = raceData.a1.fetchAndXor(1 + 8)
      consumeCPU(units: 256)
      let a2 = raceData.a2.fetchAndXor(1 + 16)
      consumeCPU(units: 256)
      let a3 = raceData.a3.xorAndFetch(1 + 32)
      consumeCPU(units: 256)
      let a4 = raceData.a4.xorAndFetch(1 + 64)
      return Observation(1, a1, a2, a3, a4)
    } else {
      // Reader.
      //
      // Observing a non-zero a1 does not impose any constraints onto
      // subsequent loads of a2, a3 or a4: since stores to a2, a3 and a4 don't
      // happen before the store to a1, there are executions where we observed
      // a non-zero a1, but storing to a2 does not happen before loading from
      // a2 (same for a3 and a4).
      consumeCPU(units: 256)
      let a1 = raceData.a1.load()
      consumeCPU(units: 256)
      let a2 = raceData.a2.load()
      consumeCPU(units: 256)
      let a3 = raceData.a3.load()
      consumeCPU(units: 256)
      let a4 = raceData.a4.load()

      return Observation(2, a1, a2, a3, a4)
    }
  }

  func evaluateObservations(
    _ observations: [Observation],
    _ sink: (RaceTestObservationEvaluation) -> Void
  ) {
    for observation in observations {
      switch observation {
      case Observation(1,  3,  3, 34, 66),
           Observation(2,  3,  3,  3,  3),
           Observation(2,  3,  3,  3, 66),
           Observation(2,  3,  3, 34,  3),
           Observation(2,  3,  3, 34, 66),
           Observation(2,  3, 18,  3,  3),
           Observation(2,  3, 18,  3, 66),
           Observation(2,  3, 18, 34,  3),
           Observation(2,  3, 18, 34, 66),
           Observation(2, 10,  3,  3,  3),
           Observation(2, 10,  3,  3, 66),
           Observation(2, 10,  3, 34,  3),
           Observation(2, 10,  3, 34, 66),
           Observation(2, 10, 18,  3,  3),
           Observation(2, 10, 18,  3, 66),
           Observation(2, 10, 18, 34,  3),
           Observation(2, 10, 18, 34, 66):
        sink(.passInteresting(String(describing: observation)))

      default:
        sink(.failureInteresting(String(describing: observation)))
      }
    }
  }
}


var dummyObjectCount = _stdlib_ShardedAtomicCounter()

struct AtomicInitializeARCRefRaceTest : RaceTestWithPerTrialData {
  class DummyObject {
    var payload: UInt = 0x12345678
    var randomInt: Int
    var destroyedFlag: HeapBool
    init(destroyedFlag: HeapBool, randomInt: Int) {
      self.destroyedFlag = destroyedFlag
      self.randomInt = randomInt
      dummyObjectCount.add(1, randomInt: self.randomInt)
    }
    deinit {
      self.destroyedFlag.value = true
      dummyObjectCount.add(-1, randomInt: self.randomInt)
    }
  }

  class RaceData {
    var _atomicReference: AnyObject? = nil

    var atomicReferencePtr: UnsafeMutablePointer<AnyObject?> {
      return _getUnsafePointerToStoredProperties(self).assumingMemoryBound(
        to: Optional<AnyObject>.self)
    }

    init() {}
  }

  typealias ThreadLocalData = _stdlib_ShardedAtomicCounter.PRNG
  typealias Observation = Observation4UInt

  func makeRaceData() -> RaceData {
    return RaceData()
  }

  func makeThreadLocalData() -> ThreadLocalData {
    return ThreadLocalData()
  }

  func thread1(
    _ raceData: RaceData, _ threadLocalData: inout ThreadLocalData
  ) -> Observation {
    var observation = Observation4UInt(0, 0, 0, 0)
    let initializerDestroyed = HeapBool(false)
    do {
      let initializer = DummyObject(
        destroyedFlag: initializerDestroyed,
        randomInt: threadLocalData.randomInt())
      let wonRace = _stdlib_atomicInitializeARCRef(
        object: raceData.atomicReferencePtr, desired: initializer)
      observation.data1 = wonRace ? 1 : 0
      if let ref =
        _stdlib_atomicLoadARCRef(object: raceData.atomicReferencePtr) {
        let dummy = ref as! DummyObject
        observation.data2 = unsafeBitCast(ref, to: UInt.self)
        observation.data3 = dummy.payload
      }
    }
    observation.data4 = initializerDestroyed.value ? 1 : 0
    return observation
  }

  func evaluateObservations(
    _ observations: [Observation],
    _ sink: (RaceTestObservationEvaluation) -> Void
  ) {
    let ref = observations[0].data2
    if observations.contains(where: { $0.data2 != ref }) {
      for observation in observations {
        sink(.failureInteresting("mismatched reference, expected \(ref): \(observation)"))
      }
      return
    }
    if observations.contains(where: { $0.data3 != 0x12345678 }) {
      for observation in observations {
        sink(.failureInteresting("wrong data: \(observation)"))
      }
      return
    }

    var wonRace = 0
    var lostRace = 0
    for observation in observations {
      switch (observation.data1, observation.data4) {
      case (1, 0):
        // Won race, value not destroyed.
        wonRace += 1
      case (0, 1):
        // Lost race, value destroyed.
        lostRace += 1
      default:
        sink(.failureInteresting(String(describing: observation)))
      }
    }
    if wonRace != 1 {
      for observation in observations {
        sink(.failureInteresting("zero or more than one thread won race: \(observation)"))
      }
      return
    }
    if lostRace < 1 {
      for observation in observations {
        sink(.failureInteresting("no thread lost race: \(observation)"))
      }
      return
    }

    sink(.pass)
  }
}

struct AtomicAcquiringARCRefRaceTest: RaceTestWithPerTrialData {
  typealias DummyObject = AtomicInitializeARCRefRaceTest.DummyObject

  class RaceData {
    var _atomicReference: DummyObject? = nil

    var atomicReferencePtr: UnsafeMutablePointer<DummyObject?> {
      _getUnsafePointerToStoredProperties(self).assumingMemoryBound(
        to: Optional<DummyObject>.self)
    }

    init() {}
  }

  typealias ThreadLocalData = _stdlib_ShardedAtomicCounter.PRNG
  typealias Observation = Observation4UInt

  func makeRaceData() -> RaceData {
    RaceData()
  }

  func makeThreadLocalData() -> ThreadLocalData {
    ThreadLocalData()
  }

  func thread1(
    _ raceData: RaceData, _ threadLocalData: inout ThreadLocalData
  ) -> Observation {
    var observation = Observation4UInt(0, 0, 0, 0)
    let initializerDestroyed = HeapBool(false)
    do {
      let object = DummyObject(
        destroyedFlag: initializerDestroyed,
        randomInt: threadLocalData.randomInt())
      let value = Unmanaged.passUnretained(object)
      let result = _stdlib_atomicAcquiringInitializeARCRef(
        object: raceData.atomicReferencePtr, desired: object)
      observation.data1 = (result.toOpaque() == value.toOpaque()  ? 1 : 0)
      if let loaded =
        _stdlib_atomicAcquiringLoadARCRef(object: raceData.atomicReferencePtr) {
        observation.data2 = UInt(bitPattern: loaded.toOpaque())
        observation.data3 = loaded._withUnsafeGuaranteedRef { $0.payload }
      }
    }
    observation.data4 = initializerDestroyed.value ? 1 : 0
    return observation
  }

  func evaluateObservations(
    _ observations: [Observation],
    _ sink: (RaceTestObservationEvaluation) -> Void
  ) {
    let ref = observations[0].data2
    if observations.contains(where: { $0.data2 != ref }) {
      for observation in observations {
        sink(.failureInteresting("mismatched reference, expected \(ref): \(observation)"))
      }
      return
    }
    if observations.contains(where: { $0.data3 != 0x12345678 }) {
      for observation in observations {
        sink(.failureInteresting("wrong data: \(observation)"))
      }
      return
    }

    var wonRace = 0
    var lostRace = 0
    for observation in observations {
      switch (observation.data1, observation.data4) {
      case (1, 0):
        // Won race, value not destroyed.
        wonRace += 1
      case (0, 1):
        // Lost race, value destroyed.
        lostRace += 1
      default:
        sink(.failureInteresting(String(describing: observation)))
      }
    }
    if wonRace != 1 {
      for observation in observations {
        sink(.failureInteresting("zero or more than one thread won race: \(observation)"))
      }
      return
    }
    if lostRace < 1 {
      for observation in observations {
        sink(.failureInteresting("no thread lost race: \(observation)"))
      }
      return
    }

    sink(.pass)
  }
}

var AtomicIntTestSuite = TestSuite("AtomicInt")

AtomicIntTestSuite.test("fetchAndAdd/1") {
  runRaceTest(AtomicInt_fetchAndAdd_1_RaceTest.self,
    operations: 6400, timeoutInSeconds: 60)
}

AtomicIntTestSuite.test("fetchAndAdd/ReleaseAtomicStores/1") {
  runRaceTest(
    AtomicInt_fetchAndAdd_ReleaseAtomicStores_1_RaceTest.self,
    operations: 12800, timeoutInSeconds: 60)
}

AtomicIntTestSuite.test("fetchAndAdd/ReleaseAtomicStores/2") {
  runRaceTest(
    AtomicInt_fetchAndAdd_ReleaseAtomicStores_2_RaceTest.self,
    operations: 12800, timeoutInSeconds: 60)
}

AtomicIntTestSuite.test("fetchAndAdd/ReleaseNonAtomicStores/1") {
  runRaceTest(
    AtomicInt_fetchAndAdd_ReleaseNonAtomicStores_RaceTest.self,
    operations: 25600, timeoutInSeconds: 60)
}

AtomicIntTestSuite.test("fetchAndAnd/1") {
  runRaceTest(AtomicInt_fetchAndAnd_1_RaceTest.self,
    operations: 6400, timeoutInSeconds: 60)
}
// FIXME: add more tests for fetchAndAnd, like we have for fetchAndAdd.

AtomicIntTestSuite.test("fetchAndOr/1") {
  runRaceTest(AtomicInt_fetchAndOr_1_RaceTest.self,
    operations: 6400, timeoutInSeconds: 60)
}
// FIXME: add more tests for fetchAndOr, like we have for fetchAndAdd.

AtomicIntTestSuite.test("fetchAndXor/1") {
  runRaceTest(AtomicInt_fetchAndXor_1_RaceTest.self,
    operations: 6400, timeoutInSeconds: 60)
}
// FIXME: add more tests for fetchAndXor, like we have for fetchAndAdd.


var AtomicARCRefTestSuite = TestSuite("AtomicARCRef")

AtomicARCRefTestSuite.test("seqcst_initialize,load") {
  runRaceTest(AtomicInitializeARCRefRaceTest.self,
    operations: 25600, timeoutInSeconds: 60)
  expectEqual(0, dummyObjectCount.getSum())
}

AtomicARCRefTestSuite.test("acquire_initialize,load") {
  runRaceTest(AtomicAcquiringARCRefRaceTest.self,
    operations: 25600, timeoutInSeconds: 60)
  expectEqual(0, dummyObjectCount.getSum())
}

runAllTests()
