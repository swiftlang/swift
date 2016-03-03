// RUN: %target-resilience-test --no-backward-deployment
// REQUIRES: executable_test

import StdlibUnittest
import protocol_add_requirements

// Also import modules which are used by StdlibUnittest internally. This
// workaround is needed to link all required libraries in case we compile
// StdlibUnittest with -sil-serialize-all.
import SwiftPrivate
import SwiftPrivatePthreadExtras
#if _runtime(_ObjC)
import ObjectiveC
#endif

var ProtocolAddRequirementsTest = TestSuite("ProtocolAddRequirements")

struct Halogen : ElementProtocol {
  var x: Int

  func increment() -> Halogen {
    return Halogen(x: x + 1)
  }
}

func ==(h1: Halogen, h2: Halogen) -> Bool {
  return h1.x == h2.x
}

struct ConformingType : AddRequirementsProtocol {
  func importantOperation() -> Halogen {
    return Halogen(x: 0)
  }

#if AFTER
  func unimportantOperation() -> Halogen {
    return Halogen(x: 10)
  }
#endif
}

ProtocolAddRequirementsTest.test("AddRequirements") {
  let result = doSomething(ConformingType())

#if BEFORE
  let expected = [0, 1, 2].map(Halogen.init)
#else
  let expected = [0, 10, 11].map(Halogen.init)
#endif

  expectEqual(result, expected)
}

runAllTests()

