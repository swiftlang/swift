// RUN: %target-run-simple-swift

import StdlibUnittest

enum ClericalErrorDomain: _ErrorType {
  case MisplacedDocument(name: String)
  case AccidentallyErasedTape(fromMinute: Double, toMinute: Double)
}

var EnumErrorType = TestSuite("Enum ErrorType derivation")

EnumErrorType.test("default codes") {
  let a: ClericalErrorDomain = .MisplacedDocument(name: "check-in times.doc")
  let b: ClericalErrorDomain
    = .AccidentallyErasedTape(fromMinute: 5, toMinute: 23.5)
  expectEqual(a.domain, "main.ClericalErrorDomain")
  expectEqual(b.domain, "main.ClericalErrorDomain")

  expectEqual(a.code, 0)
  expectEqual(b.code, 1)
}
