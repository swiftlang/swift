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
    expectFalse(s.isBase)
    let sc: BaseT = cast(s)
    expectFalse(sc.isBase)
    let sx: BaseT = cxxCast(s)      // should instantiate I to SubT and O to BaseT
    expectFalse(sc.isBase)
}

TemplatingTestSuite.test("BaseT") {
    let b: BaseT = BaseT.getBaseT()
    expectTrue(b.isBase)
    let bc: BaseT = cxxCast(b)      // should instantiate I and O both to BaseT
    expectTrue(bc.isBase)
}

TemplatingTestSuite.test("DerivedOutOfOrder") {
    let d = DerivedOutOfOrder.getInstance()
    expectEqual(123, d.baseField)
    expectEqual(456, d.derivedField)
    expectEqual(789, d.leafField)
}

runAllTests()
