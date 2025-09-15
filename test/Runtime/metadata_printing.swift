// RUN: %empty-directory(%t)
// RUN: %target-build-swift -target %target-future-triple -parse-stdlib %s -module-name main -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out
// REQUIRES: executable_test
// REQUIRES: concurrency
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import Swift
import StdlibUnittest
import _Concurrency

let MetadataPrintingTests = TestSuite("MetadataPrinting")

class NonSendable {
  var x: Int = 0
}

func expect<T>(type: T.Type, printsAs string: String) {
  expectEqual(string, "\(type)")
}

MetadataPrintingTests.test("@isolated(any) functions") {
  expect(type: (@isolated(any) () -> ()).self,
         printsAs: "@isolated(any) () -> ()")
  expect(type: (@isolated(any) () -> NonSendable).self,
         printsAs: "@isolated(any) () -> NonSendable")
  expect(type: (@isolated(any) () -> sending NonSendable).self,
         printsAs: "@isolated(any) () -> sending NonSendable")
}

MetadataPrintingTests.test("global actor functions") {
  expect(type: (@MainActor () -> ()).self,
         printsAs: "@MainActor () -> ()")
  expect(type: (@MainActor () -> NonSendable).self,
         printsAs: "@MainActor () -> NonSendable")
  expect(type: (@MainActor () -> sending NonSendable).self,
         printsAs: "@MainActor () -> sending NonSendable")
}

runAllTests()
