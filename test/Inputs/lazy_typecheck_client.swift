// This source file contains an example client of all the public declarations
// exposed by the library implemented in lazy_typecheck.swift.

import lazy_typecheck

struct ConformsToPublicProto: PublicProto {
  func req() -> Int { return 1 }
}

struct ConformsToMainActorProto: MainActorProtocol {
  func req() -> Int { return 1 }
}

func testGlobalFunctions() {
  _ = publicFunc()
  _ = publicFuncWithDefaultArg()
  #if TEST_PACKAGE
  _ = packageFunc()
  #endif
  constrainedGenericPublicFunction(ConformsToPublicProto())
  _ = publicSpecializedFunc(4)
  _ = publicSpecializedFunc(ConformsToPublicProto())
  if #available(SwiftStdlib 5.1, *) {
    _ = publicFuncWithOpaqueReturnType()
    _ = publicAEICFuncWithOpaqueReturnType()
  }
}

func testGobalVars() {
  let _: Int = publicGlobalVar
  let _: String = publicGlobalVarInferredType
  let _: (Int, Int) = (publicGlobalVarInferredTuplePatX, publicGlobalVarInferredTuplePatY)
}

func testPublicStruct() {
  let s = PublicStruct(x: 1)
  let _: Int = s.publicMethod()
  let _: Int = s.publicProperty
  let _: String = s.publicPropertyInferredType
  let _: Double = s.publicWrappedProperty
  let _: Double = s.$publicWrappedProperty.wrappedValue
  PublicStruct.publicStaticMethod()
  PublicStruct.activeMethod()
}

func testPublicClasses() {
  let c = PublicClass(x: 2)
  let _: Int = c.publicMethod()
  let _: Int = c.publicProperty
  let _: String = c.publicPropertyInferredType
  PublicClass.publicClassMethod()

  let d = PublicDerivedClass(x: 3)
  let _: Int = d.publicMethod()
  let _: Int = d.publicProperty
  let _: String = d.publicPropertyInferredType
  PublicDerivedClass.publicClassMethod()

  class DerivedFromPublicClassSynthesizedDesignatedInit: PublicClassSynthesizedDesignatedInit {
    init() {}
  }
  let _ = DerivedFromPublicClassSynthesizedDesignatedInit()
}

func testPublicEnum(_ e: PublicEnum) {
  switch e {
  case .a: ()
  case .b(let x): let _: Int = x
  }

  let _: Int = e.publicMethod()
  let _: Int = e.publicComputedVar
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

@MainActor
func testMainActorConstraint() {
  let _: ConformsToMainActorProto = ConformsToMainActorProto()
  let _: Int = PublicStruct(x: 5).publicMainActorMethod()
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

func testTypealiases() {
  let _: PublicStruct = PublicStructAlias(x: 1)
}

func testOperators() {
  var a: PublicStruct
  a <<< PublicStruct(x: 2)
}
