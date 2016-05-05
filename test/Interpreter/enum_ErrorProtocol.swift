// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest


enum ClericalErrorDomain: ErrorProtocol {
  case MisplacedDocument(name: String)
  case AccidentallyErasedTape(fromMinute: Double, toMinute: Double)
}

enum EmptyErrorDomain: ErrorProtocol {}

var EnumErrorProtocol = TestSuite("Enum ErrorProtocol derivation")

EnumErrorProtocol.test("default codes") {
  let a: ClericalErrorDomain = .MisplacedDocument(name: "check-in times.doc")
  let b: ClericalErrorDomain
    = .AccidentallyErasedTape(fromMinute: 5, toMinute: 23.5)
  expectEqual(a._domain, "main.ClericalErrorDomain")
  expectEqual(b._domain, "main.ClericalErrorDomain")

  expectEqual(a._code, 0)
  expectEqual(b._code, 1)
}

runAllTests()
