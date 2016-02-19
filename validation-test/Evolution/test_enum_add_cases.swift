// RUN: %target-resilience-test
// REQUIRES: executable_test

import StdlibUnittest
import enum_add_cases

// Also import modules which are used by StdlibUnittest internally. This
// workaround is needed to link all required libraries in case we compile
// StdlibUnittest with -sil-serialize-all.
import SwiftPrivate
import SwiftPrivatePthreadExtras
#if _runtime(_ObjC)
import ObjectiveC
#endif

var EnumAddCasesTest = TestSuite("EnumAddCases")

func myAddNoPayloadToSingletonCases() -> [AddNoPayloadToSingleton] {
  return [.Noses]
}

func evaluateAddNoPayloadToSingletonCases(e: [AddNoPayloadToSingleton]) -> [Int] {
  return e.map {
    switch $0 {
    case .Noses:
      return 0
    default:
      return -1
    }
  }
}

EnumAddCasesTest.test("AddNoPayloadToSingleton") {
  expectEqual([0],
      evaluateAddNoPayloadToSingletonCases(myAddNoPayloadToSingletonCases()))
  if getVersion() == 0 {
    expectEqual([0],
        evaluateAddNoPayloadToSingletonCases(addNoPayloadToSingletonCases()))
  } else {
    expectEqual([0, -1, -1],
        evaluateAddNoPayloadToSingletonCases(addNoPayloadToSingletonCases()))
  }
}

///////////////////////////////////////////////////////////////////////

func myAddPayloadToSingletonCases() -> [AddPayloadToSingleton] {
  return [.Cats]
}

func evaluateAddPayloadToSingletonCases(e: [AddPayloadToSingleton]) -> [Int] {
  return e.map {
    switch $0 {
    case .Cats:
      return 0
    default:
      return -1
    }
  }
}

EnumAddCasesTest.test("AddPayloadToSingleton") {
  do {
    let s = Starfish()

    expectEqual([0],
        evaluateAddPayloadToSingletonCases(myAddPayloadToSingletonCases()))
    if getVersion() == 0 {
      expectEqual([0],
          evaluateAddPayloadToSingletonCases(addPayloadToSingletonCases(s)))
    } else {
      expectEqual([0, -1],
          evaluateAddPayloadToSingletonCases(addPayloadToSingletonCases(s)))
    }
  }

  expectEqual(starfishCount, 0)
}

///////////////////////////////////////////////////////////////////////

func myAddNoPayloadToSinglePayloadCases(s: Starfish)
    -> [AddNoPayloadToSinglePayload] {
  return [.Cats(s), .Noses]
}

func evaluateAddNoPayloadToSinglePayloadCases(s: Starfish,
                                            _ e: [AddNoPayloadToSinglePayload])
    -> [Int] {
  return e.map {
    switch $0 {
    case .Cats(let ss):
      expectTrue(s === ss)
      return 0
    case .Noses:
      return 1
    default:
      return -1
    }
  }
}

EnumAddCasesTest.test("AddNoPayloadToSinglePayload") {
  do {
    let s = Starfish()
    expectEqual([0, 1],
        evaluateAddNoPayloadToSinglePayloadCases(s, myAddNoPayloadToSinglePayloadCases(s)))
    if getVersion() == 0 {
      expectEqual([0, 1],
          evaluateAddNoPayloadToSinglePayloadCases(s, addNoPayloadToSinglePayloadCases(s)))
    } else {
      expectEqual([0, 1, -1],
          evaluateAddNoPayloadToSinglePayloadCases(s, addNoPayloadToSinglePayloadCases(s)))
    }
  }
  expectEqual(starfishCount, 0)
}

///////////////////////////////////////////////////////////////////////

func myAddPayloadToSinglePayloadCases(s: Starfish)
    -> [AddPayloadToSinglePayload] {
  return [.Cats, .Paws(s)]
}

func evaluateAddPayloadToSinglePayloadCases(s: Starfish,
                                            _ e: [AddPayloadToSinglePayload])
    -> [Int] {
  return e.map {
    switch $0 {
    case .Cats:
      return 0
    case .Paws(let ss):
      expectTrue(s === ss)
      return 1
    default:
      return -1
    }
  }
}

EnumAddCasesTest.test("AddPayloadToSinglePayload") {
  do {
    let s = Starfish()
    expectEqual([0, 1],
        evaluateAddPayloadToSinglePayloadCases(s, myAddPayloadToSinglePayloadCases(s)))
    if getVersion() == 0 {
      expectEqual([0, 1],
          evaluateAddPayloadToSinglePayloadCases(s, addPayloadToSinglePayloadCases(s)))
    } else {
      expectEqual([0, 1, -1],
          evaluateAddPayloadToSinglePayloadCases(s, addPayloadToSinglePayloadCases(s)))
    }
  }
  expectEqual(starfishCount, 0)
}

///////////////////////////////////////////////////////////////////////

func myAddNoPayloadToMultiPayloadCases(s: Starfish)
    -> [AddNoPayloadToMultiPayload] {
  return [.Cats(s), .Puppies(s)]
}

func evaluateAddNoPayloadToMultiPayloadCases(s: Starfish,
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
    default:
      return -1
    }
  }
}

EnumAddCasesTest.test("AddNoPayloadToMultiPayload") {
  do {
    let s = Starfish()
    expectEqual([0, 1],
        evaluateAddNoPayloadToMultiPayloadCases(s, myAddNoPayloadToMultiPayloadCases(s)))
    if getVersion() == 0 {
      expectEqual([0, 1],
          evaluateAddNoPayloadToMultiPayloadCases(s, addNoPayloadToMultiPayloadCases(s)))
    } else {
      expectEqual([0, 1, -1, -1],
          evaluateAddNoPayloadToMultiPayloadCases(s, addNoPayloadToMultiPayloadCases(s)))
    }
  }
  expectEqual(starfishCount, 0)
}

///////////////////////////////////////////////////////////////////////

func myAddPayloadToMultiPayloadCases(s: Starfish)
    -> [AddPayloadToMultiPayload] {
  return [.Cats(s), .Ponies(s), .Pandas]
}

func evaluateAddPayloadToMultiPayloadCases(s: Starfish,
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
    default:
      return -1
    }
  }
}

EnumAddCasesTest.test("AddPayloadToMultiPayload") {
  do {
    let s = Starfish()
    expectEqual([0, 1, 2],
        evaluateAddPayloadToMultiPayloadCases(s, myAddPayloadToMultiPayloadCases(s)))
    if getVersion() == 0 {
      expectEqual([0, 1, 2],
          evaluateAddPayloadToMultiPayloadCases(s, addPayloadToMultiPayloadCases(s)))
    } else {
      expectEqual([0, 1, 2, -1],
          evaluateAddPayloadToMultiPayloadCases(s, addPayloadToMultiPayloadCases(s)))
    }
  }
  expectEqual(starfishCount, 0)
}

runAllTests()

