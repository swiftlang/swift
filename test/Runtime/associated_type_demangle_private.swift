// RUN: %empty-directory(%t)
// RUN: %target-build-swift -parse-stdlib %s -module-name main -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out
// REQUIRES: executable_test

import Swift
import StdlibUnittest

protocol P {
  associatedtype A
}

fileprivate struct Foo {
  fileprivate struct Inner: P {
    fileprivate struct Innermost { }
    typealias A = Innermost
  }
}

func getP_A<T: P>(_: T.Type) -> Any.Type {
  return T.A.self
}

let AssociatedTypeDemangleTests = TestSuite("AssociatedTypeDemangle")


AssociatedTypeDemangleTests.test("private types") {
  expectEqual(Foo.Inner.Innermost.self, getP_A(Foo.Inner.self))
}

runAllTests()
