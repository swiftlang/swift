// RUN: %target-resilience-test
// REQUIRES: executable_test

import StdlibUnittest
import enum_add_cases_trap


var EnumAddCasesTest = TestSuite("EnumAddCasesTrap")

extension TestSuite {
  func testCrashOnNewLibrary(_ typeName: String, code: @escaping () -> Void) {
    let newTest = self.test(typeName)
    if getVersion() == 0 {
      newTest.code(code)
    } else {
      newTest.crashOutputMatches("'\(typeName)'").code {
        expectCrashLater()
        code()
      }
    }
  }
}


func evaluateAddNoPayloadToSingletonCases(_ e: [AddNoPayloadToSingleton]) -> [Int] {
  return e.map {
    switch $0 {
    case .Noses:
      return 0
#if AFTER
    default:
      fatalError("dummy error for '\(type(of: $0))'")
#endif
    }
  }
}

EnumAddCasesTest.testCrashOnNewLibrary("AddNoPayloadToSingleton") {
  if getVersion() == 0 {
    expectEqual([0],
        evaluateAddNoPayloadToSingletonCases(addNoPayloadToSingletonCases()))
  } else {
    _ = evaluateAddNoPayloadToSingletonCases(addNoPayloadToSingletonCases())
  }
}

///////////////////////////////////////////////////////////////////////

func evaluateAddPayloadToSingletonCases(_ e: [AddPayloadToSingleton]) -> [Int] {
  return e.map {
    switch $0 {
    case .Cats:
      return 0
#if AFTER
    default:
      fatalError("dummy error for '\(type(of: $0))'")
#endif
    }
  }
}

EnumAddCasesTest.testCrashOnNewLibrary("AddPayloadToSingleton") {
  do {
    let s = Starfish()

    if getVersion() == 0 {
      expectEqual([0],
          evaluateAddPayloadToSingletonCases(addPayloadToSingletonCases(s)))
    } else {
      _ = evaluateAddPayloadToSingletonCases(addPayloadToSingletonCases(s))
    }
  }

  expectEqual(starfishCount, 0)
}

///////////////////////////////////////////////////////////////////////

func evaluateAddNoPayloadToSinglePayloadCases(_ s: Starfish,
                                            _ e: [AddNoPayloadToSinglePayload])
    -> [Int] {
  return e.map {
    switch $0 {
    case .Cats(let ss):
      expectTrue(s === ss)
      return 0
    case .Noses:
      return 1
#if AFTER
    default:
      fatalError("dummy error for '\(type(of: $0))'")
#endif
    }
  }
}

EnumAddCasesTest.testCrashOnNewLibrary("AddNoPayloadToSinglePayload") {
  do {
    let s = Starfish()
    if getVersion() == 0 {
      expectEqual([0, 1],
          evaluateAddNoPayloadToSinglePayloadCases(s, addNoPayloadToSinglePayloadCases(s)))
    } else {
      _ = evaluateAddNoPayloadToSinglePayloadCases(s, addNoPayloadToSinglePayloadCases(s))
    }
  }
  expectEqual(starfishCount, 0)
}

///////////////////////////////////////////////////////////////////////

func evaluateAddPayloadToSinglePayloadCases(_ s: Starfish,
                                            _ e: [AddPayloadToSinglePayload])
    -> [Int] {
  return e.map {
    switch $0 {
    case .Cats:
      return 0
    case .Paws(let ss):
      expectTrue(s === ss)
      return 1
#if AFTER
    default:
      fatalError("dummy error for '\(type(of: $0))'")
#endif
    }
  }
}

EnumAddCasesTest.testCrashOnNewLibrary("AddPayloadToSinglePayload") {
  do {
    let s = Starfish()

    if getVersion() == 0 {
      expectEqual([0, 1],
          evaluateAddPayloadToSinglePayloadCases(s, addPayloadToSinglePayloadCases(s)))
    } else {
      _ = evaluateAddPayloadToSinglePayloadCases(s, addPayloadToSinglePayloadCases(s))
    }
  }
  expectEqual(starfishCount, 0)
}

///////////////////////////////////////////////////////////////////////

func evaluateAddNoPayloadToMultiPayloadCases(_ s: Starfish,
                                            _ e: [AddNoPayloadToMultiPayload])
    -> [Int] {
  return e.map {
    switch $0 {
    case .Cats(let ss):
      expectTrue(s === ss)
      return 0
    case .Puppies(let ss):
      expectTrue(s === ss)
      return 1
#if AFTER
    default:
      fatalError("dummy error for '\(type(of: $0))'")
#endif
    }
  }
}

EnumAddCasesTest.testCrashOnNewLibrary("AddNoPayloadToMultiPayload") {
  do {
    let s = Starfish()

    if getVersion() == 0 {
      expectEqual([0, 1],
          evaluateAddNoPayloadToMultiPayloadCases(s, addNoPayloadToMultiPayloadCases(s)))
    } else {
      _ = evaluateAddNoPayloadToMultiPayloadCases(s, addNoPayloadToMultiPayloadCases(s))
    }
  }
  expectEqual(starfishCount, 0)
}


///////////////////////////////////////////////////////////////////////

func evaluateAddPayloadToMultiPayloadCases(_ s: Starfish,
                                            _ e: [AddPayloadToMultiPayload])
    -> [Int] {
  return e.map {
    switch $0 {
    case .Cats(let ss):
      expectTrue(s === ss)
      return 0
    case .Ponies(let ss):
      expectTrue(s === ss)
      return 1
    case .Pandas:
      return 2
#if AFTER
    default:
      fatalError("dummy error for '\(type(of: $0))'")
#endif
    }
  }
}

EnumAddCasesTest.testCrashOnNewLibrary("AddPayloadToMultiPayload") {
  do {
    let s = Starfish()

    if getVersion() == 0 {
      expectEqual([0, 1, 2],
          evaluateAddPayloadToMultiPayloadCases(s, addPayloadToMultiPayloadCases(s)))
    } else {
      _ = evaluateAddPayloadToMultiPayloadCases(s, addPayloadToMultiPayloadCases(s))
    }
  }
  expectEqual(starfishCount, 0)
}

runAllTests()

