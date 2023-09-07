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
  let s = PublicStruct(x: 1)
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

// FIXME: This conformance ought to be included to verify that a redundant
// conformance diagnostic is emitted.
// However, it turns out that the mechanism implemented by
// https://github.com/apple/swift/pull/20433 doesn't actually work when a
// module is built from .swiftinterface because the dummy conformance is marked
// unavailable.
//extension PublicGenericStruct: EmptyPublicProto {}

func takesEmptyProto<T: EmptyPublicProto>(_ t: T) {}

@available(*, unavailable)
func testConditionalConformance<T>(_ s: PublicGenericStruct<T>) {
  takesEmptyProto(s) // expected-error {{global function 'takesEmptyProto' requires}}
}
