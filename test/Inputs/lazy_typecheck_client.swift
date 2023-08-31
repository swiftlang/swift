// This source file contains an example client of all the public declarations
// exposed by the library implemented in lazy_typecheck.swift.

import lazy_typecheck

struct ConformsToPublicProto: PublicProto {
  func req() -> Int { return 1 }
}

func testGlobalFunctions() {
  _ = publicFunc()
  _ = publicFuncWithDefaultArg()
  #if TEST_PACKAGE
  _ = packageFunc()
  #endif
  constrainedGenericPublicFunction(ConformsToPublicProto())
  if #available(SwiftStdlib 5.1, *) {
    _ = publicFuncWithOpaqueReturnType()
    _ = publicAEICFuncWithOpaqueReturnType()
  }
}

func testPublicStruct(_ s: PublicStruct) {
  _ = s.publicMethod()
}
