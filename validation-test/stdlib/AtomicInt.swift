// RUN: rm -rf %t
// RUN: mkdir -p %t
//
// RUN: %target-build-swift -Xfrontend -disable-access-control -module-name a %s -o %t.out -O
// RUN: %target-run %t.out

import Swift
import StdlibUnittest

class AtomicInt4RaceData {
  var writerStarted = _stdlib_AtomicInt(0)
  var a1: _stdlib_AtomicInt
  var a2: _stdlib_AtomicInt
  var a3: _stdlib_AtomicInt

  init(_ a1: Int, _ a2: Int, _ a3: Int) {
    self.a1 = _stdlib_AtomicInt(a1)
    self.a2 = _stdlib_AtomicInt(a2)
    self.a3 = _stdlib_AtomicInt(a3)
  }
}

struct AtomicIntRaceTest : RaceTestWithPerTrialDataType {
  typealias RaceData = AtomicInt4RaceData
  typealias ThreadLocalData = Void
  typealias Observation = Observation4Word

  func makeRaceData() -> RaceData {
    return RaceData(0, 0, 0)
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
      return Observation(1, a1, a2, a3)
    } else {
      // Reader.
      let a1 = raceData.a1.load()
      let a2 = raceData.a2.load()
      let a3 = raceData.a3.load()
      return Observation(2, a1, a2, a3)
    }
  }

  func evaluateObservations<
    S : SinkType where S.Element == RaceTestObservationEvaluation
  >(observations: [Observation], inout _ sink: S) {
    for observation in observations {
      switch observation {
      case Observation(1, 0, 0, 30),
           Observation(2, 0, 0, 0),
           Observation(2, 10, 0, 0),
           Observation(2, 10, 20, 0),
           Observation(2, 10, 20, 30):
        sink.put(.PassInteresting(toString(observation)))

      default:
        sink.put(.FailureInteresting(toString(observation)))
      }
    }
  }
}

class HeapBool {
  var value: Bool
  init(_ value: Bool) {
    self.value = value
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
      if let ref: AnyObject =
        _stdlib_atomicLoadARCRef(object: raceData.atomicReferencePtr) {
        let dummy = ref as DummyObject
        observation.uw2 = unsafeBitCast(ref, UWord.self)
        observation.uw3 = dummy.payload
      }
    }
    observation.uw4 = initializerDestroyed.value ? 1 : 0
    return observation
  }

  func evaluateObservations<
    S : SinkType where S.Element == RaceTestObservationEvaluation
  >(observations: [Observation], inout _ sink: S) {
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

AtomicIntTestSuite.test("fetchAndAdd") {
  runRaceTest(AtomicIntRaceTest.self, trials: 100)
}

var AtomicARCRefTestSuite = TestSuite("AtomicARCRef")

AtomicARCRefTestSuite.test("initialize,load") {
  runRaceTest(AtomicInitializeARCRefRaceTest.self, trials: 100)
  expectEqual(0, dummyObjectCount.getSum())
}

runAllTests()

