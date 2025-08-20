// REQUIRES: executable_test
// RUN: %target-run-simple-swift(-cxx-interoperability-mode=default -Xfrontend -disable-availability-checking -I %S%{fs-sep}Inputs)



import Inheritance
import StdlibUnittest

// A function whose explicit type annotations specializes the cxxCast function.
//
// In Swift, the generic type parameters of cxxCast I and O should be respectively instantiated to SubT and BaseT.
// However, since these are foreign reference types, this instantiates (and calls) cxxCast<SubT *, BaseT *> in C++.
func cast(_ s: SubT) -> BaseT {
  return cxxCast(s)
}

var InheritanceTestSuite = TestSuite("Inheritance of foreign reference types")

InheritanceTestSuite.test("Templated cast to base") {
  let s: SubT = SubT.getSubT()
  expectFalse(s.isBase)
  let sc: BaseT = cast(s)
  expectFalse(sc.isBase)
  let sx: BaseT = cxxCast(s)  // should instantiate I to SubT and O to BaseT
  expectFalse(sx.isBase)
  let sy: BaseT = Foo.cxxCast(s)  // should instantiate I to SubT and O to BaseT
  expectFalse(sy.isBase)
}

InheritanceTestSuite.test("Templated cast to itself") {
  let b: BaseT = BaseT.getBaseT()
  expectTrue(b.isBase)
  let bc: BaseT = cxxCast(b)  // should instantiate I and O both to BaseT
  expectTrue(bc.isBase)
}

InheritanceTestSuite.test("DerivedOutOfOrder") {
  let d = DerivedOutOfOrder.getInstance()
  expectEqual(123, d.baseField)
  expectEqual(456, d.derivedField)
  expectEqual(789, d.leafField)
}

InheritanceTestSuite.test("DerivedUsesBaseTailPadding") {
  let d = DerivedUsesBaseTailPadding.getInstance()
  expectEqual(123, d.field8)
  expectEqual(456, d.field4)
  expectEqual(789, d.field2)
}

InheritanceTestSuite.test("ParentChild") {
  let immortalRefType = ImmortalRefereceExample.returnImmortalRefType()
  expectTrue(
    type(of: immortalRefType) is AnyObject.Type,
    "Expected immortalRefType to be a reference type, but it’s a value type")

  let derivedFromImmortalRefType = ImmortalRefereceExample.returnDerivedFromImmortalRefType()
  expectTrue(
    type(of: derivedFromImmortalRefType) is AnyObject.Type,
    "Expected derivedFromImmortalRefType to be a value type, but it’s a reference type")

  let valType = ExplicitAnnotationHasPrecedence1.returnValueType()
  expectTrue(
    !(type(of: valType) is AnyObject.Type),
    "Expected valType to be a value type, but it’s a reference type")

  let referenceType = ExplicitAnnotationHasPrecedence1.returnRefType()
  expectTrue(
    type(of: referenceType) is AnyObject.Type,
    "Expected referenceType to be a reference type, but it’s a value type")

  let derivedFromValType = ExplicitAnnotationHasPrecedence1.returnDerivedFromValueType()
  expectTrue(
    !(type(of: derivedFromValType) is AnyObject.Type),
    "Expected derivedFromValType to be a value type, but it’s a reference type")

  let derivedFromRefTypeAAndB = ExplicitAnnotationHasPrecedence2.returnDerivedFromRefTypeAAndB()
  expectTrue(
    !(type(of: derivedFromRefTypeAAndB) is AnyObject.Type),
    "Expected derivedFromRefTypeAAndB to be a value type, but it’s a reference type")

  let derivedFromRefTypeAAndBAnnotated =
    ExplicitAnnotationHasPrecedence2.returnDerivedFromRefTypeAAndBAnnotated()
  expectTrue(
    type(of: derivedFromRefTypeAAndBAnnotated) is AnyObject.Type,
    "Expected derivedFromRefTypeAAndBAnnotated to be a reference type, but it’s a value type")

  let derivedFromValueTypeAndAnnotated =
    ExplicitAnnotationHasPrecedence1.returnDerivedFromValueTypeAndAnnotated()
  expectTrue(
    type(of: derivedFromValueTypeAndAnnotated) is AnyObject.Type,
    "Expected derivedFromValueTypeAndAnnotated to be a reference type, but it’s a value type")

  let derivedFromReferenceType = ExplicitAnnotationHasPrecedence1.returnDerivedFromRefType()
  expectTrue(
    type(of: derivedFromReferenceType) is AnyObject.Type,
    "Expected derivedFromReferenceType to be a reference type, but it’s a value type")

  let derivedFromReferenceTypeAndAnnotated =
    ExplicitAnnotationHasPrecedence1.returnDerivedFromRefTypeAndAnnotated()
  expectTrue(
    type(of: derivedFromReferenceTypeAndAnnotated) is AnyObject.Type,
    "Expected derivedFromReferenceTypeAndAnnotated to be a reference type, but it’s a value type"
  )

  let valueType = BasicInheritanceExample.returnValueType()
  expectTrue(
    !(type(of: valueType) is AnyObject.Type),
    "Expected valueType to be a value type, but it’s a reference type")

  let refType = BasicInheritanceExample.returnRefType()
  expectTrue(
    type(of: refType) is AnyObject.Type,
    "Expected refType to be a reference type, but it’s a value type")

  let derivedFromRefType = BasicInheritanceExample.returnDerivedFromRefType()
  expectTrue(
    type(of: derivedFromRefType) is AnyObject.Type,
    "Expected derivedFromRefType to be a reference type, but it’s a value type")

  let derivedFromBaseRef1AndBaseRef2 =
    MultipleInheritanceExample1.returnDerivedFromBaseRef1AndBaseRef2()
  expectTrue(
    !(type(of: derivedFromBaseRef1AndBaseRef2) is AnyObject.Type),
    "Expected derivedFromBaseRef1AndBaseRef2 to be a value type, but it’s a reference type")

  let derivedFromBaseRef3 = MultipleInheritanceExample1.returnDerivedFromBaseRef3()
  expectTrue(
    type(of: derivedFromBaseRef3) is AnyObject.Type,
    "Expected derivedFromBaseRef3 to be a reference type, but it’s a value type")

  let d2 = MultipleInheritanceExample2.returnD()
  expectTrue(
    !(type(of: d2) is AnyObject.Type),
    "Expected d2 to be a value type, but it’s a reference type")

  let d3 = MultipleInheritanceExample2.returnD()
  expectTrue(
    !(type(of: d3) is AnyObject.Type),
    "Expected d3 to be a value type, but it’s a reference type")

  let refTypeDiamond = RefTypeDiamondInheritance.returnDiamond()
  expectTrue(
    !(type(of: refTypeDiamond) is AnyObject.Type),
    "Expected refTypeDiamond to be a value type, but it’s a reference type")

  let virtualDiamond = RefTypeDiamondInheritance.returnVirtualDiamond()
  expectTrue(
    type(of: virtualDiamond) is AnyObject.Type,
    "Expected virtualDiamond to be a reference type, but it’s a value type")

  let nonRefTypeDiamond = NonRefTypeDiamondInheritance.returnDiamond()
  expectTrue(
    type(of: nonRefTypeDiamond) is AnyObject.Type,
    "Expected nonRefTypeDiamond to be a reference type, but it’s a value type")

  let forest = InheritingTemplatedRefType.returnForest()
  expectTrue(
    type(of: forest) is AnyObject.Type,
    "Expected forest to be a reference type, but it’s a value type")
}

runAllTests()
