// RUN: rm -rf %t
// RUN: mkdir -p %t
//
// RUN: %target-build-swift -Xfrontend -disable-access-control -module-name a %s -o %t.out -O
// RUN: %target-run %t.out

// XFAIL: linux

import Swift
import StdlibUnittest

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

struct AtomicInt_fetchAndAdd_1_RaceTest : RaceTestWithPerTrialDataType {
  typealias RaceData = AtomicInt4RaceData
  typealias ThreadLocalData = Void
  typealias Observation = Observation5Word

  func makeRaceData() -> RaceData {
    return RaceData(0, 0, 0, 0)
  }

  func makeThreadLocalData() -> Void {
    return Void()
  }

  func thread1(
    raceData: RaceData, inout _ threadLocalData: ThreadLocalData
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

  func evaluateObservations<
    S : SinkType where S.Element == RaceTestObservationEvaluation
  >(observations: _UnitTestArray<Observation>, inout _ sink: S) {
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
        sink.put(.PassInteresting(toString(observation)))

      default:
        sink.put(.FailureInteresting(toString(observation)))
      }
    }
  }
}

struct AtomicInt_fetchAndAdd_ReleaseAtomicStores_1_RaceTest
  : RaceTestWithPerTrialDataType {

  typealias RaceData = AtomicInt4RaceData
  typealias ThreadLocalData = Void
  typealias Observation = Observation5Word

  func makeRaceData() -> RaceData {
    return RaceData(0, 0, 0, 0)
  }

  func makeThreadLocalData() -> Void {
    return Void()
  }

  func thread1(
    raceData: RaceData, inout _ threadLocalData: ThreadLocalData
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

  func evaluateObservations<
    S : SinkType where S.Element == RaceTestObservationEvaluation
  >(observations: _UnitTestArray<Observation>, inout _ sink: S) {
    for observation in observations {
      switch observation {
      case Observation(1,  0,  0, 30, 40),
           Observation(2,  0,  0,  0,  0),
           Observation(2, 10,  0,  0,  0),
           Observation(2, 10, 20,  0,  0),
           Observation(2, 10, 20, 30,  0),
           Observation(2, 10, 20, 30, 40):
        sink.put(.PassInteresting(toString(observation)))

      default:
        sink.put(.FailureInteresting(toString(observation)))
      }
    }
  }
}

struct AtomicInt_fetchAndAdd_ReleaseAtomicStores_2_RaceTest
  : RaceTestWithPerTrialDataType {

  typealias RaceData = AtomicInt4RaceData
  typealias ThreadLocalData = Void
  typealias Observation = Observation5Word

  func makeRaceData() -> RaceData {
    return RaceData(0, 0, 0, 0)
  }

  func makeThreadLocalData() -> Void {
    return Void()
  }

  func thread1(
    raceData: RaceData, inout _ threadLocalData: ThreadLocalData
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

  func evaluateObservations<
    S : SinkType where S.Element == RaceTestObservationEvaluation
  >(observations: _UnitTestArray<Observation>, inout _ sink: S) {
    for observation in observations {
      switch observation {
      case Observation(1,  0,  0, 30, 40),
           Observation(2,  0,  0,  0,  0),
           Observation(2, 10,  0,  0,  0),
           Observation(2, 10, 20,  0,  0),
           Observation(2, 10, 20, 30,  0),
           Observation(2, 10, 20, 30, 40):
        sink.put(.PassInteresting(toString(observation)))

      default:
        sink.put(.FailureInteresting(toString(observation)))
      }
    }
  }
}

struct AtomicInt_fetchAndAdd_ReleaseNonAtomicStores_RaceTest
  : RaceTestWithPerTrialDataType {

  typealias RaceData = AtomicInt4HeapInt2Int2RaceData
  typealias ThreadLocalData = Void
  typealias Observation = Observation9Word

  func makeRaceData() -> RaceData {
    return RaceData(0, 0, 0, 0, 0, 0, 0, 0)
  }

  func makeThreadLocalData() -> Void {
    return Void()
  }

  func thread1(
    raceData: RaceData, inout _ threadLocalData: ThreadLocalData
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

  func evaluateObservations<
    S : SinkType where S.Element == RaceTestObservationEvaluation
  >(observations: _UnitTestArray<Observation>, inout _ sink: S) {
    for observation in observations {
      switch observation {
      case Observation(1,  0,  0, 30, 40, 100, 200, 300, 400),
           Observation(2,  0,  0,  0,  0, 999, 999, 999, 999),
           Observation(2, 10,  0,  0,  0, 100, 999, 999, 999),
           Observation(2, 10, 20,  0,  0, 100, 200, 999, 999),
           Observation(2, 10, 20, 30,  0, 100, 200, 300, 999),
           Observation(2, 10, 20, 30, 40, 100, 200, 300, 400):
        sink.put(.PassInteresting(toString(observation)))

      default:
        sink.put(.FailureInteresting(toString(observation)))
      }
    }
  }
}


var dummyObjectCount = _stdlib_ShardedAtomicCounter()

struct AtomicInitializeARCRefRaceTest : RaceTestWithPerTrialDataType {
  class DummyObject {
    var payload: UWord = 0x12345678
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
      return UnsafeMutablePointer(_getUnsafePointerToStoredProperties(self))
    }

    init() {}
  }

  typealias ThreadLocalData = _stdlib_ShardedAtomicCounter.PRNG
  typealias Observation = Observation4UWord

  func makeRaceData() -> RaceData {
    return RaceData()
  }

  func makeThreadLocalData() -> ThreadLocalData {
    return ThreadLocalData()
  }

  func thread1(
    raceData: RaceData, inout _ threadLocalData: ThreadLocalData
  ) -> Observation {
    var observation = Observation4UWord(0, 0, 0, 0)
    var initializerDestroyed = HeapBool(false)
    if true {
      let initializer = DummyObject(
        destroyedFlag: initializerDestroyed,
        randomInt: threadLocalData.randomInt())
      let wonRace = _stdlib_atomicInitializeARCRef(
        object: raceData.atomicReferencePtr, desired: initializer)
      observation.uw1 = wonRace ? 1 : 0
      if let ref =
        _stdlib_atomicLoadARCRef(object: raceData.atomicReferencePtr) {
        let dummy = ref as! DummyObject
        observation.uw2 = unsafeBitCast(ref, UWord.self)
        observation.uw3 = dummy.payload
      }
    }
    observation.uw4 = initializerDestroyed.value ? 1 : 0
    return observation
  }

  func evaluateObservations<
    S : SinkType where S.Element == RaceTestObservationEvaluation
  >(observations: _UnitTestArray<Observation>, inout _ sink: S) {
    let ref = observations[0].uw2
    if contains(observations, { $0.uw2 != ref }) {
      for observation in observations {
        sink.put(.FailureInteresting("mismatched reference, expected \(ref): \(observation)"))
      }
      return
    }
    if contains(observations, { $0.uw3 != 0x12345678 }) {
      for observation in observations {
        sink.put(.FailureInteresting("wrong data: \(observation)"))
      }
      return
    }

    var wonRace = 0
    var lostRace = 0
    for observation in observations {
      switch (observation.uw1, observation.uw4) {
      case (1, 0):
        // Won race, value not destroyed.
        ++wonRace
      case (0, 1):
        // Lost race, value destroyed.
        ++lostRace
      default:
        sink.put(.FailureInteresting(toString(observation)))
      }
    }
    if wonRace != 1 {
      for observation in observations {
        sink.put(.FailureInteresting("zero or more than one thread won race: \(observation)"))
      }
      return
    }
    if lostRace < 1 {
      for observation in observations {
        sink.put(.FailureInteresting("no thread lost race: \(observation)"))
      }
      return
    }

    sink.put(.Pass)
  }
}

var AtomicIntTestSuite = TestSuite("AtomicInt")

AtomicIntTestSuite.test("fetchAndAdd/1") {
  runRaceTest(AtomicInt_fetchAndAdd_1_RaceTest.self, trials: 100)
}

AtomicIntTestSuite.test("fetchAndAdd/ReleaseAtomicStores/1") {
  runRaceTest(
    AtomicInt_fetchAndAdd_ReleaseAtomicStores_1_RaceTest.self,
    trials: 200)
}

AtomicIntTestSuite.test("fetchAndAdd/ReleaseAtomicStores/2") {
  runRaceTest(
    AtomicInt_fetchAndAdd_ReleaseAtomicStores_2_RaceTest.self,
    trials: 200)
}

AtomicIntTestSuite.test("fetchAndAdd/ReleaseNonAtomicStores/1") {
  runRaceTest(
    AtomicInt_fetchAndAdd_ReleaseNonAtomicStores_RaceTest.self,
    trials: 400)
}

var AtomicARCRefTestSuite = TestSuite("AtomicARCRef")

AtomicARCRefTestSuite.test("initialize,load") {
  runRaceTest(AtomicInitializeARCRefRaceTest.self, trials: 400)
  expectEqual(0, dummyObjectCount.getSum())
}

runAllTests()

