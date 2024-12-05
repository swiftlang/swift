// REQUIRES: executable_test
// RUN: %target-run-simple-swift(-cxx-interoperability-mode=default -Xfrontend -disable-availability-checking -I %S/Inputs)

import StdlibUnittest
import Inheritance

// A function whose explicit type annotations specializes the cxxCast function.
//
// In Swift, the generic type parameters of cxxCast I and O should be respectively instantiated to SubT and BaseT.
// However, since these are foreign reference types, this instantiates (and calls) cxxCast<SubT *, BaseT *> in C++.
func cast(_ s: SubT) -> BaseT {
    return cxxCast(s)
}

var TemplatingTestSuite = TestSuite("Foreign references work with templates")

TemplatingTestSuite.test("SubT") {
    let s: SubT = SubT.getSubT()
    expectTrue(!s.isBase)
    let sc: BaseT = cast(s)
    expectTrue(!sc.isBase)
    let sx: BaseT = cxxCast(s)      // should instantiate I to SubT and O to BaseT
    expectTrue(!sx.isBase)
}

TemplatingTestSuite.test("BaseT") {
    let b: BaseT = BaseT.getBaseT()
    expectTrue(b.isBase)
    let bc: BaseT = cxxCast(b)      // should instantiate I and O both to BaseT
    expectTrue(bc.isBase)
}

var FrtInheritanceTestSuite = TestSuite("Foreign references in C++ inheritance")

FrtInheritanceTestSuite.test("ParentChild") {
  var x = returnValueType()
  var y = returnRefType()
  var z = returnDerivedFromRefType()
  expectTrue(!(type(of: x) is AnyObject.Type), "Expected x to be a value type, but it’s a reference type")
  expectTrue(type(of: y) is AnyObject.Type, "Expected y to be a reference type, but it’s a value type")
  expectTrue(type(of: z) is AnyObject.Type, "Expected z to be a reference type, but it’s a value type")
}

runAllTests()
