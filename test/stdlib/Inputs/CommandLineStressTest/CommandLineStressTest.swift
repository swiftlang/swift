// Do not change the SIL name for this without also changing CommandLineStressTest.c
@_cdecl("swift_commandline_test_getProcessArgs")
public func runTest() {
  let CommandLineRaceTestSuite = TestSuite("CommandLine Race")

  CommandLineRaceTestSuite.test("passes") {
    runRaceTest(CommandLineRace.self, trials: 1)
  }

  runAllTests()
}

import StdlibUnittest

struct CommandLineRace : RaceTestWithPerTrialData {
  class CommandLineRaceData {
    init() {}
  }

  typealias ThreadLocalData = Void
  typealias Observation = Observation1UInt

  func makeRaceData() -> CommandLineRaceData {
    return CommandLineRaceData()
  }

  func makeThreadLocalData() -> Void {
    return Void()
  }

  func thread1(
    _ raceData: CommandLineRaceData, _ threadLocalData: inout ThreadLocalData
  ) -> Observation {
    let argptr = CommandLine.unsafeArgv
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
