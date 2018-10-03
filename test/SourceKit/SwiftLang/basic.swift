// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: OS=macosx
// REQUIRES: objc_interop

import StdlibUnittest
import SwiftLang

func getSyntaxMap() -> SourceKitdResponse.Array {
  let service = SourceKitdService()
  let request = SourceKitdRequest(uid: .request_EditorOpen)

  request.addParameter(.key_Name, value: "foo")
  request.addParameter(.key_SourceText, value: "print(\"Hello, world!\")")
  request.addParameter(.key_EnableSyntaxMap, value: 1)
  request.addParameter(.key_SyntacticOnly, value: 1)

  return service.sendSyn(request: request).value.getArray(.key_SyntaxMap)
}

var swiftLangVariantTests = TestSuite("SwiftLangVariantTests")

swiftLangVariantTests.test("VariantLifetime") {
  let syntaxMap = getSyntaxMap()
  expectEqual(2, syntaxMap.count)
}

runAllTests()
