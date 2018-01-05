// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

let DemangleToMetadataTests = TestSuite("DemangleToMetadata")


DemangleToMetadataTests.test("malformed mangled names") {
  expectNil(_typeByMangledName("blah"))
}

DemangleToMetadataTests.test("tuple types") {
  expectEqual(type(of: ()), _typeByMangledName("yt")!)
  expectEqual(type(of: ((), ())), _typeByMangledName("yt_ytt")!)
  expectEqual(type(of: ((), b: ())), _typeByMangledName("yt_yt1bt")!)
  expectEqual(type(of: (a: (), ())), _typeByMangledName("yt1a_ytt")!)
  expectEqual(type(of: (a: (), b: ())), _typeByMangledName("yt1a_yt1bt")!)
}

runAllTests()

