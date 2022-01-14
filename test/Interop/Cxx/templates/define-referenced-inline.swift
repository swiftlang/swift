// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-cxx-interop -Xcc -fno-exceptions)
//
// REQUIRES: executable_test
//
// Enable this everywhere once we have a solution for modularizing libstdc++: rdar://87654514
// REQUIRES: OS=macosx

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
