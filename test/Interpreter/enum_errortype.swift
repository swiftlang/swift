// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

// Also import modules which are used by StdlibUnittest internally. This
// workaround is needed to link all required libraries in case we compile
// StdlibUnittest with -sil-serialize-all.
import SwiftPrivate
#if _runtime(_ObjC)
import ObjectiveC
#endif

enum ClericalErrorDomain: ErrorType {
  case MisplacedDocument(name: String)
  case AccidentallyErasedTape(fromMinute: Double, toMinute: Double)
}

enum EmptyErrorDomain: ErrorType {}

var EnumErrorType = TestSuite("Enum ErrorType derivation")

EnumErrorType.test("default codes") {
  let a: ClericalErrorDomain = .MisplacedDocument(name: "check-in times.doc")
  let b: ClericalErrorDomain
    = .AccidentallyErasedTape(fromMinute: 5, toMinute: 23.5)
  expectEqual(a._domain, "main.ClericalErrorDomain")
  expectEqual(b._domain, "main.ClericalErrorDomain")

  expectEqual(a._code, 0)
  expectEqual(b._code, 1)
}

runAllTests()
