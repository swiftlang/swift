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

func testPublicStruct() {
  var s = PublicStruct(x: 1)
  _ = s.publicMethod()
  PublicStruct.publicStaticMethod()
}

func testPublicClass() {
  let c = PublicClass(x: 2)
  _ = c.publicMethod()
  PublicClass.publicClassMethod()

  let d = PublicDerivedClass(x: 3)
  _ = d.publicMethod()
  PublicDerivedClass.publicClassMethod()
}

func testConformances() {
  let array: [any PublicProto] = [
    PublicStructConformingToPublicProto(),
    PublicStructIndirectlyConformingToPublicProto(),
    PublicClassConformingToPublicProto(),
    "string",
    PublicClassInheritingConformanceToPublicProto(),
  ]

  for x in array {
    _ = x.req()
    constrainedGenericPublicFunction(x)
  }
}
