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
  typealias Observation = Observation4Word

  func makeRaceData() -> RaceData {
    return RaceData(0, 0, 0)
  }

  func thread1(raceData: RaceData) -> Observation {
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
        sink.put(.Failure)
      }
    }
  }
}

var AtomicIntTestSuite = TestSuite("AtomicInt")

AtomicIntTestSuite.test("fetchAndAdd") {
  runRaceTest(AtomicIntRaceTest.self, trials: 100)
}

runAllTests()

