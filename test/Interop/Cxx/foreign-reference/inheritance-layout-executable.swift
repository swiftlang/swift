// REQUIRES: executable_test
// RUN: %target-run-simple-swift(-cxx-interoperability-mode=default -Xfrontend -disable-availability-checking -I %S/Inputs)

import InheritanceLayout
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

  expectTrue(type(of: s) is AnyObject.Type, "SubT should be a reference type")
  expectTrue(type(of: sc) is AnyObject.Type, "BaseT should be a reference type")
  expectTrue(type(of: sx) is AnyObject.Type, "BaseT should be a reference type")
  expectTrue(type(of: sy) is AnyObject.Type, "BaseT should be a reference type")
}

InheritanceTestSuite.test("Templated cast to itself") {
  let b: BaseT = BaseT.getBaseT()
  expectTrue(b.isBase)
  let bc: BaseT = cxxCast(b)  // should instantiate I and O both to BaseT
  expectTrue(bc.isBase)

  expectTrue(type(of: b) is AnyObject.Type, "BaseT should be a reference type")
  expectTrue(type(of: bc) is AnyObject.Type, "BaseT should be a reference type")
}

InheritanceTestSuite.test("DerivedOutOfOrder") {
  let d = DerivedOutOfOrder.getInstance()
  expectEqual(123, d.baseField)
  expectEqual(456, d.derivedField)
  expectEqual(789, d.leafField)
  expectTrue(type(of: d) is AnyObject.Type, "DerivedOutOfOrder should be a reference type")
}

InheritanceTestSuite.test("DerivedUsesBaseTailPadding") {
  let d = DerivedUsesBaseTailPadding.getInstance()
  expectEqual(123, d.field8)
  expectEqual(456, d.field4)
  expectEqual(789, d.field2)
  expectTrue(type(of: d) is AnyObject.Type, "DerivedUsesBaseTailPadding should be a reference type")
}

runAllTests()
