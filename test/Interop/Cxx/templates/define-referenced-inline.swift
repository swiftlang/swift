// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-experimental-cxx-interop -Xcc -fno-exceptions -Xfrontend -disable-availability-checking)
//
// REQUIRES: executable_test

import DefineReferencedInline
import StdlibUnittest

var TemplatesTestSuite = TestSuite("TemplatesTestSuite")

TemplatesTestSuite.test("tests") {
  let a = CtorCallsInlineFn<CInt>(0)
  let b = HasInlineDtor<CInt>()
  let c = ParentWithChildWithInlineCtorDtor<CInt>()
  let d = HolderWithChildWithInlineCtorDtor<CInt>()
  let e = DtorCallsInlineMethod<CInt>()
}

runAllTests()
