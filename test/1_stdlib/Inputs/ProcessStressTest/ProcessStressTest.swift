// Do not change the SIL name for this without also changing ProcessStressTest.c
@_silgen_name("swift_process_test_getProcessArgs")
public func runTest() {
  let ProcessRaceTestSuite = TestSuite("Process Race")

  ProcessRaceTestSuite.test("passes") {
    runRaceTest(ProcessRace.self, trials: 1)
  }

  runAllTests()
}

import StdlibUnittest

struct ProcessRace : RaceTestWithPerTrialData {
  class ProcessRaceData {
    init() {}
  }

  typealias ThreadLocalData = Void
  typealias Observation = Observation1UInt

  func makeRaceData() -> ProcessRaceData {
    return ProcessRaceData()
  }

  func makeThreadLocalData() -> Void {
    return Void()
  }

  func thread1(
    _ raceData: ProcessRaceData, _ threadLocalData: inout ThreadLocalData
  ) -> Observation {
    let argptr = Process.unsafeArgv
    return Observation(unsafeBitCast(argptr, to: UInt.self))
  }

  func evaluateObservations(
    _ observations: [Observation],
    _ sink: (RaceTestObservationEvaluation) -> Void
  ) {
    guard let fstObs = observations.first?.data1 else {
      return
    }
    for observation in observations {
      if observation.data1 == fstObs {
        sink(.pass)
      } else {
        sink(.failure)
      }
    }
  }
}
